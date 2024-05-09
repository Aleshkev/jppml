{-# LANGUAGE FlexibleContexts #-}

module Typecheck where

import AbsSyntax (Con, Con' (CInt, CString, CUnit), Dec, Dec' (DExn, DLet, DOpen, DType), ECaseBind, ECaseBind' (ECBJust), Exp, Exp' (EApp, ECase, ECon, EFn, EId, ELet, EObjCon, ETup), Id (Id), IdCap (IdCap), IdVar (IdVar), LetBind, Pat, Pat' (PCon, PId, PObj, PObjCon, PTup, PWild), Typ, Typ' (TFn, TId, TIdVar, TTup), TypBind' (TBJust))
import AbsUtil (Src, dtagId, dtagTyp, exnBindId, exnBindTyp, letBindExp, letBindId, printPosition, srcOf, stringToDecs, tTupElems, typBindId, typBindTypLst, typLstToTyps, typsOfTypLst)
import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import qualified Data.Foldable as Set
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.Set as Set
import Infer (Type (RTerm, RVar), TypeEq, collectVars, simplifyVars, substituteTypes, unifyAll)
import ParSyntax (myLexer, pTyp)
import Util (catMaybesFst, existDuplicates, foldInserter, isReserved)
import Data.List (find)

newtype TypeDec = TypeDec (Map.Map String [Type])

data TypeEnv = TypeEnv
  { -- Bound values (these which have a concrete value).
    binds :: Map.Map String Type
  , -- Free values (these which are currently being resolved).
    -- Note that if a value is here, it already shadows the previous bound value.
    freeBinds :: Map.Map String Type
  }

insertBind :: String -> Type -> TypeEnv -> TypeEnv
insertBind name typ env =
  env{binds = binds env & Map.insert name typ}

insertFreeBind :: String -> Type -> TypeEnv -> TypeEnv
insertFreeBind name typ env =
  env{freeBinds = freeBinds env & Map.insert name typ}

insertBinds :: [(String, Type)] -> TypeEnv -> TypeEnv
insertBinds = foldInserter insertBind

insertFreeBinds :: [(String, Type)] -> TypeEnv -> TypeEnv
insertFreeBinds = foldInserter insertFreeBind

emptyTEnv :: TypeEnv
emptyTEnv = TypeEnv{binds = Map.empty, freeBinds = Map.empty}

data TypeState = TypeState
  { -- What type does a constructor require, and what type does it belong to. Type is in the form "RTerm ('a, 'b) name"
    constructorToType :: Map.Map String (Maybe Type, String)
  , types :: Map.Map String Int -- Used as list of types, and how many type vars each type has.
  , globBinds :: Map.Map String Type
  , nextVarI :: Int
  }

insertGlobBind :: String -> Type -> TypeState -> TypeState
insertGlobBind id t state = state{globBinds = globBinds state & Map.insert id t}

insertGlobBinds :: [(String, Type)] -> TypeState -> TypeState
insertGlobBinds = foldInserter insertGlobBind

emptyTypeState :: TypeState
emptyTypeState =
  TypeState
    { globBinds = Map.empty
    , nextVarI = 0
    , constructorToType = Map.empty
    , types = Map.fromList [("__fn", 2), ("int", 0), ("string", 0), ("unit", 0)]
    }

type TypecheckM = ReaderT TypeEnv (StateT TypeState (ExceptT (String, Src) IO))

runTypecheckM :: TypecheckM a -> TypeState -> IO (Either (String, Src) (a, TypeState))
runTypecheckM f state = do
  runExceptT $ runStateT (runReaderT f emptyTEnv) state

freshVar :: TypecheckM Type
freshVar = do
  varI <- gets nextVarI
  let name = "'__a" ++ show varI
  modify (\state -> state{nextVarI = varI + 1})
  return $ RVar name

refreshVars :: Type -> TypecheckM Type
refreshVars t = do
  let a = collectVars t
  b <- mapM (const freshVar) a
  return $ t & substituteTypes (zip a b)

insertBuiltinType :: String -> String -> TypecheckM ()
insertBuiltinType name spec = do
  let t = typToType $ fromRight (error $ "syntax error in builtin type: " ++ show spec) (pTyp $ myLexer spec)
  modify (insertGlobBind name t)
  return ()

typecheckCon :: Con -> TypecheckM Type
typecheckCon x = case x of
  CInt _ _ -> return $ RTerm [] "int"
  CString _ _ -> return $ RTerm [] "string"
  CUnit _ -> return $ RTerm [] "unit"

-- Type check either the "let" declaration or the "let" expression.
typecheckLet :: Src -> [LetBind] -> Maybe Exp -> TypecheckM ([(String, Type)], Maybe Type, [TypeEq])
typecheckLet p letbinds innerExp = do
  let (ids, valExps) = (map letBindId letbinds, map letBindExp letbinds)
  when (ids & catMaybes & existDuplicates) $
    throwError ("multiple bindings for a symbol", p)
  protBind <- asks (\env -> ids & catMaybes & filter isReserved & find (\x -> binds env & Map.member x))
  when (isJust protBind) $
    throwError ("redefinition of a protected symbol " ++ fromJust protBind, p)

  valTs <- mapM (const freshVar) valExps
  let valBinds = zip ids valTs & catMaybesFst
  eqs <-
    local
      (insertFreeBinds valBinds)
      ( foldlM
          ( \acc (valExp, valT) -> do
              (valT', acc') <- typecheckExp valExp
              return $ (valT, valT', p) : acc' ++ acc
          )
          []
          (zip valExps valTs)
      )
  case unifyAll eqs of
    Left s -> throwError s
    Right subst -> do
      let newValTs = valTs & map (simplifyVars . substituteTypes subst)
      let newValBinds = zip ids newValTs & catMaybesFst
      case innerExp of
        Nothing -> return (newValBinds, Nothing, [])
        Just innerExp -> do
          (t, eqs) <- local (insertBinds newValBinds) (typecheckExp innerExp)
          return (newValBinds, Just t, eqs)

typecheckExp :: Exp -> TypecheckM (Type, [TypeEq])
typecheckExp = \case
  ECon p con -> do
    t <- freshVar
    conT <- typecheckCon con
    return (t, [(t, conT, p)])
  EObjCon p (IdCap idcap) -> typecheckExp (EId p (Id idcap))
  EId p (Id id) -> do
    v <- freshVar
    tFree <- asks (\env -> freeBinds env & Map.lookup id)
    case tFree of
      Just t -> return (v, [(t, v, p)])
      Nothing -> do
        tBound <- asks (\env -> binds env & Map.lookup id)
        case tBound of
          Just t -> do
            t' <- refreshVars t
            return (v, [(t', v, p)])
          Nothing -> throwError ("undefined symbol '" ++ id ++ "'", p)
  ETup p exp exps -> do
    t <- freshVar
    ts <- mapM typecheckExp (exp : exps)
    return (t, (t, RTerm (map fst ts) "__tuple", p) : concatMap snd ts)
  EApp p exp1 exp2 -> do
    rett <- freshVar
    (fnt, fndeps) <- typecheckExp exp1
    (argt, argdeps) <- typecheckExp exp2
    return (rett, (fnt, RTerm [argt, rett] "__fn", p) : fndeps ++ argdeps)
  ELet p letbinds exp -> do
    (_, Just t, deps) <- typecheckLet p letbinds (Just exp)
    return (t, deps)
  ECase p exp ecasebinds -> do
    (argT, argdeps) <- typecheckExp exp
    retT <- freshVar
    (patTs, expTs, deps) <- mapM typecheckECaseBind ecasebinds & fmap unzip3
    let makeEqDeps = map (argT,,p) patTs ++ map (,retT,p) expTs
    return (retT, makeEqDeps ++ argdeps ++ concat deps)
  EFn p [Id id] retExp -> do
    fnT <- freshVar
    argT <- freshVar
    when (isReserved id) $ throwError ("can't define symbol '" ++ id ++ "' in a function argument", p)
    (retT, retdeps) <- local (insertFreeBind id argT) (typecheckExp retExp)
    return (fnT, (fnT, RTerm [argT, retT] "__fn", p) : retdeps)
  _ -> error "unexpected expression at type checking stage"

typecheckECaseBind :: ECaseBind -> TypecheckM (Type, Type, [TypeEq])
typecheckECaseBind x = case x of
  ECBJust _ pat exp -> do
    (patT, patBinds, patDeps) <- typecheckPat pat
    (expT, expDeps) <- local (insertFreeBinds $ Map.toList patBinds) (typecheckExp exp)
    return (patT, expT, patDeps ++ expDeps)

type PatBinds = Map.Map String Type

-- result type, values bound by the pattern match, constraints
typecheckPat :: Pat -> TypecheckM (Type, PatBinds, [TypeEq])
typecheckPat x = case x of
  PCon _ con -> do
    conT <- typecheckCon con
    return (conT, Map.empty, [])
  PId p (Id id) -> do
    t <- freshVar
    when (isReserved id) $ throwError ("can't define symbol '" ++ id ++ "' in a pattern", p)
    return (t, Map.singleton id t, [])
  PWild _ -> do
    t <- freshVar
    return (t, Map.empty, [])
  PTup p pat pats -> do
    p' <- mapM typecheckPat (pat : pats)
    let (elemTs, binds, deps) = unzip3 p'
    v <- freshVar
    binds' <- concatPatBinds p binds
    return (v, binds', (v, RTerm elemTs "__tuple", p) : concat deps)
  PObjCon p (IdCap idcap) -> typecheckPatObj p idcap Nothing
  PObj p (IdCap idcap) pat -> typecheckPatObj p idcap (Just pat)
  _ -> error "unexpected pattern at type checking stage"

typecheckPatObj :: Src -> String -> Maybe Pat -> TypecheckM (Type, PatBinds, [TypeEq])
typecheckPatObj p idcap usedArgPat = do
  v <- freshVar
  constrT <- gets (\state -> globBinds state & Map.lookup idcap)
  case (constrT, usedArgPat) of
    (Just (RTerm _ "__fn"), Just provArgPat) -> do
      constrT' <- refreshVars (fromJust constrT)
      let (RTerm [expectedArgT, retT] "__fn") = constrT'
      (provArgT, argbinds, argdeps) <- typecheckPat provArgPat
      return (v, argbinds, (v, retT, p) : (expectedArgT, provArgT, p) : argdeps)
    (Just (RTerm _ "__fn"), Nothing) -> throwError ("constructor '" ++ idcap ++ "' requires an argument", p)
    (Just retT, Nothing) -> do
      retT' <- refreshVars retT
      return (v, Map.empty, [(v, retT', p)])
    (Just _, Just _) -> throwError ("constructor '" ++ idcap ++ "' doesn't accept an argument", p)
    (Nothing, _) -> throwError ("undefined variant '" ++ idcap ++ "'", p)

concatPatBinds :: Src -> [PatBinds] -> TypecheckM PatBinds
concatPatBinds p ms = do
  let keys = concatMap Map.keys ms
  when (keys & existDuplicates) $ throwError ("duplicate bindings in a pattern", p)
  return $ foldl Map.union Map.empty ms

typToType :: Typ -> Type
typToType x = case x of
  TIdVar _ (IdVar idvar) -> RVar idvar
  TId _ typlst (Id id) ->
    RTerm (typLstToTyps typlst & map typToType) id
  TTup _ typ ttupelems -> do
    let elemTs = typ : tTupElems ttupelems & map typToType
    RTerm elemTs "__tuple"
  TFn _ typ1 typ2 -> do
    let ts = [typ1, typ2] & map typToType
    RTerm ts "__fn"

typecheckTyp :: Src -> Set.Set String -> Typ -> TypecheckM Type
typecheckTyp p vars t = do
  let t' = typToType t
  walk t'
  return t'
 where
  walk :: Type -> TypecheckM ()
  walk = \case
    RVar id -> checkTypeVarExists id
    RTerm vs "__tuple" -> mapM_ walk vs
    RTerm vs id -> do
      gets (\state -> types state & Map.lookup id) >>= \case
        Nothing -> throwUndefinedType id
        Just vcount -> checkParams id vcount (length vs)
      mapM_ walk vs
  checkTypeVarExists id =
    unless (vars & Set.member id) $ throwError ("undefined type variable " ++ id ++ show vars, p)
  throwUndefinedType id =
    throwError ("undefined type '" ++ id ++ "'", p)
  checkParams id nExpect nUsed = do
    when (nExpect /= nUsed) $
      throwError ("expected " ++ id ++ " to be parametrized by " ++ show nExpect ++ " type variables, not " ++ show nUsed, p)

typecheckDec :: Dec -> TypecheckM ()
typecheckDec (DLet p letbinds) = do
  gBinds <- gets globBinds
  (newGlobBinds, Nothing, []) <- local (\env -> env{binds = gBinds}) (typecheckLet p letbinds Nothing)
  modify (insertGlobBinds newGlobBinds)
typecheckDec (DType p typbinds) = do
  vars <- mapM (\x -> typsOfTypLst (typBindTypLst x) & mapM getParamVar) typbinds
  let ids = map typBindId typbinds
  when (ids & existDuplicates) $ throwError ("multiple definitions of a type", p)
  mapM_ (uncurry insertType) (zip ids (map length vars))
  mapM_
    (\(TBJust _ _ (Id typeId) dtags, vars) -> mapM_ (insertTag typeId vars) dtags)
    (zip typbinds vars)
 where
  getParamVar = \case
    TIdVar _ (IdVar x) -> return x
    x -> throwError ("a type can be parametrized only by type variables", srcOf x)
  insertType id nVars = do
    exists <- gets (\state -> types state & Map.member id)
    when exists $ throwError ("redefinition of type '" ++ id ++ "'", p)
    modify (\state -> state{types = types state & Map.insert id nVars})
  insertTag typeId typeVars dtag = do
    constrT <- dtagTyp dtag & mapM (typecheckTyp (srcOf dtag) (Set.fromList typeVars))
    insertConstructor (dtagId dtag) constrT typeVars typeId (srcOf dtag)
typecheckDec (DExn _ exnbinds) = do
  mapM_ insertBind exnbinds
 where
  insertBind exnbind = do
    constrT <- exnBindTyp exnbind & mapM (typecheckTyp (srcOf exnbind) Set.empty)
    insertConstructor (exnBindId exnbind) constrT [] "__exn" (srcOf exnbind)
typecheckDec (DOpen p idcaps) = do
  mapM_ (\(IdCap id) -> openOne id) idcaps
 where
  openOne id = do
    source <- liftIO $ try (readFile ("src/" ++ id ++ ".ml"))
    case (source :: Either IOError String) of
      Left exn -> throwError ("can't load module '" ++ id ++ "': " ++ show exn, p)
      Right s -> case stringToDecs s of
        Left exn -> throwError ("in module " ++ id ++ ": " ++ exn, p)
        Right decs ->
          catchError
            (typecheckDecLst decs)
            (\(exn, p') -> throwError ("in module " ++ id ++ " at " ++ printPosition p' ++ ": " ++ exn, p))

-- Inserts a global function or value constructing an object. Does not allow redefinitions.
insertConstructor :: String -> Maybe Type -> [String] -> String -> Src -> TypecheckM ()
insertConstructor constrId t typeVars typeId p = do
  checkForRedefinition p
  let typeVars' = map RVar typeVars
  let constrT = case t of
        Nothing -> RTerm typeVars' typeId
        Just x -> RTerm [x, RTerm typeVars' typeId] "__fn"
  modify (insertGlobBind constrId constrT)
  modify (\state -> state{constructorToType = constructorToType state & Map.insert constrId (t, typeId)})
 where
  checkForRedefinition p = do
    does <- gets (\s -> globBinds s & Map.lookup constrId & isJust)
    when does $ throwError ("redefinition of constructor '" ++ constrId ++ "'", p)

typecheckDecLst :: [Dec] -> TypecheckM ()
typecheckDecLst = mapM_ typecheckDec
