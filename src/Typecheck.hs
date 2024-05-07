{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Typecheck where

import AbsSyntax
import AbsSyntax (BNFC'Position)
import AbsUtil (dtagId, dtagTyp, exnBindId, exnBindTyp, fromIdCap, letBindExp, letBindId, printPosition, stringToDecs, typsOfTypLst)
import Control.Exception (try)
import Control.Monad (liftM, when)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (find, intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Infer (Src, Type (RTerm, RVar), TypeEq, collectVars, simplifyVars, substituteTypes, unifyAll)
import ParSyntax (myLexer, pTyp)
import PrintSyntax (printTree)
import Util (catMaybesFst, existDuplicates)

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
insertBinds t env = foldl (\acc (k, v) -> insertBind k v acc) env t

insertFreeBinds :: [(String, Type)] -> TypeEnv -> TypeEnv
insertFreeBinds t env = foldl (\acc (k, v) -> insertFreeBind k v acc) env t

emptyTEnv :: TypeEnv
emptyTEnv = TypeEnv{binds = Map.empty, freeBinds = Map.empty}

data TypeState = TypeState
  { -- What type does a constructor require, and what type does it belong to. Type is in the form "RTerm ('a, 'b) name"
    constructorToType :: Map.Map String (Maybe Type, String)
  , globBinds :: Map.Map String Type
  , nextVarI :: Integer
  }

insertGlobBind :: String -> Type -> TypecheckM ()
insertGlobBind id t = do
  modify (\state -> state{globBinds = globBinds state & Map.insert id t})

insertGlobBinds :: [(String, Type)] -> TypecheckM ()
insertGlobBinds = mapM_ (uncurry insertGlobBind)

emptyTypeState :: TypeState
emptyTypeState = TypeState{globBinds = Map.empty, nextVarI = 0, constructorToType = Map.empty}

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

typecheckCon :: Con -> TypecheckM Type
typecheckCon x = case x of
  CInt _ _ -> return $ RTerm [] "int"
  CString _ _ -> return $ RTerm [] "string"
  CUnit _ -> return $ RTerm [] "unit"

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
    expT <- freshVar
    valTs <- mapM (const $ freshVar) letbinds
    let ids = map letBindId letbinds
    when (ids & catMaybes & existDuplicates) $ throwError ("multiple bindings for a symbol", p)
    let valBinds = zip ids valTs & catMaybesFst
    bindsAcc <-
      local
        (insertFreeBinds valBinds)
        ( foldlM
            ( \acc (letbind, bindT) -> do
                (valT, valAcc) <- typecheckExp (letBindExp letbind)
                return $ (valT, bindT, p) : valAcc ++ acc
            )
            []
            (zip letbinds valTs)
        )
    (innerT, innerAcc) <- local (insertFreeBinds valBinds) (typecheckExp exp)
    return (expT, (expT, innerT, p) : innerAcc ++ bindsAcc)
  ECase p exp ecasebinds -> do
    (argT, argdeps) <- typecheckExp exp
    retT <- freshVar
    (patTs, expTs, deps) <- mapM typecheckECaseBind ecasebinds & fmap unzip3
    let makeEqDeps = map (,argT,p) patTs ++ map (,retT,p) expTs
    return (retT, makeEqDeps ++ argdeps ++ concat deps)
  EFn p [Id id] retExp -> do
    fnT <- freshVar
    argT <- freshVar
    (retT, retdeps) <- local (insertFreeBind id argT) (typecheckExp retExp)
    return (fnT, (fnT, RTerm [argT, retT] "__fn", p) : retdeps)
  _ -> error "unexpected expression"

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
    return (t, Map.singleton id t, [])
  PWild p -> do
    t <- freshVar
    return (t, Map.empty, [])
  PTup p pat pats -> do
    p' <- mapM typecheckPat (pat : pats)
    let (elemTs, binds, deps) = unzip3 p'
    v <- freshVar
    binds' <- concatPatBinds binds
    return (v, binds', (v, RTerm elemTs "__tuple", p) : concat deps)
  PObjCon p (IdCap idcap) -> typecheckPatObj p idcap Nothing
  PObj p (IdCap idcap) pat -> typecheckPatObj p idcap (Just pat)
  _ -> error "unexpected pattern"

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

concatPatBinds :: [PatBinds] -> TypecheckM PatBinds
concatPatBinds ms = do
  let keys = concatMap Map.keys ms
  when (length keys /= length (nub keys)) $ throwError ("duplicate bindings in a pattern", Nothing)
  return $ foldl Map.union Map.empty ms

typToType :: Typ -> Type
typToType x = case x of
  TIdVar _ (IdVar idvar) -> RVar idvar
  TId _ typlst (Id id) ->
    RTerm (typLstToTypes typlst) id
  TTup _ typ ttupelems -> do
    let elemTs = map typToType (typ : map (\(TTupJust _ x) -> x) ttupelems)
    RTerm elemTs "__tuple"
  TFn _ typ1 typ2 -> do
    let ts = map typToType [typ1, typ2]
    RTerm ts "__fn"

typLstToTypes :: TypLst -> [Type]
typLstToTypes x = case x of
  TLEmpty _ -> []
  TLOne _ typ -> map typToType [typ]
  TLMany _ typ typs -> map typToType (typ : typs)

typecheckTyp :: Typ -> TypecheckM Type
typecheckTyp t = do
  let t' = typToType t
  -- TODO: check if types are actually defined
  return t'

typecheckDec :: Dec -> TypecheckM ()
typecheckDec (DLet p letbinds) = do
  let ids = map letBindId letbinds
  when (ids & catMaybes & existDuplicates) $ throwError ("multiple bindings for a symbol", p)
  valTs <- mapM (const freshVar) letbinds
  let valBinds = zip ids valTs & catMaybesFst
  gBinds <- gets globBinds
  eqs <-
    local
      (\env -> env{binds = gBinds, freeBinds = Map.fromList valBinds})
      ( foldlM
          ( \acc (letbind, bindT) -> do
              (valT, valAcc) <- typecheckExp (letBindExp letbind)
              return $ (valT, bindT, hasPosition letbind) : valAcc ++ acc
          )
          []
          (zip letbinds valTs)
      )
  case unifyAll eqs of
    Left s -> throwError s
    Right subst -> do
      let newValTs = valTs & map (simplifyVars . substituteTypes subst)
      insertGlobBinds $ zip ids newValTs & catMaybesFst
      return ()
typecheckDec (DType _ typbinds) = do
  mapM_
    ( \(TBJust _ typlst (Id typeId) dtags) -> do
        typeVars <- typsOfTypLst typlst & mapM getParamVar

        mapM_
          ( \dtag -> do
              let constrT = typToType <$> dtagTyp dtag
              insertConstructor (dtagId dtag) constrT typeVars typeId (hasPosition dtag)
          )
          dtags
    )
    typbinds
 where
  getParamVar :: Typ -> TypecheckM String
  getParamVar = \case
    TIdVar _ (IdVar x) -> return x
    x -> throwError ("a type can be parametrized only by type variables", hasPosition x)
typecheckDec (DExn _ exnbinds) = do
  mapM_
    ( \exnbind -> do
        let constrT = typToType <$> exnBindTyp exnbind
        insertConstructor (exnBindId exnbind) constrT [] "__exn" (hasPosition exnbind)
    )
    exnbinds
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
  assertDoesntExist p
  let typeVars' = map RVar typeVars
  let constrT = case t of
        Nothing -> RTerm typeVars' typeId
        Just x -> RTerm [x, RTerm typeVars' typeId] "__fn"
  insertGlobBind constrId constrT
  modify (\state -> state{constructorToType = constructorToType state & Map.insert constrId (t, typeId)})
 where
  assertDoesntExist :: Src -> TypecheckM ()
  assertDoesntExist p = do
    does <- gets (\s -> globBinds s & Map.lookup constrId & isJust)
    when does $ throwError ("redefinition of constructor '" ++ typeId ++ "'", p)

typecheckDecLst :: [Dec] -> TypecheckM ()
typecheckDecLst = mapM_ typecheckDec

insertBuiltinType :: String -> String -> TypecheckM ()
insertBuiltinType name spec = do
  let t = typToType $ fromRight (error $ "syntax error in builtin type: " ++ show spec) (pTyp $ myLexer spec)
  insertGlobBind name t
  return ()
