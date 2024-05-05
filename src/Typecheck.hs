{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Typecheck where

import AbsSyntax
import AbsUtil (dtagId, dtagTyp, exnBindId, exnBindTyp, letBindExp, letBindId, stringToDecs, typsOfTypLst, fromIdCap)
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)
import Infer (Type (RTerm, RVar), collectVars, simplifyVars, substituteTypes, unifyAll)
import ParSyntax (myLexer, pTyp)
import PrintSyntax (printTree)
import Util (concatMaybesFst)

newtype TypeDec = TypeDec (Map.Map String [Type])

data TypeEnv = TypeEnv
  { binds :: Map.Map String Type
  , freeBinds :: Map.Map String Type
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

data TypeStore = TypeStore
  { --Constructors which this type has.
    types :: Map.Map String [String]
  , -- What type does a constructor require, and what type does it belong to. Type is in the form RTerm ('a, 'b) name
    constructorToType :: Map.Map String (Maybe Type, String)
  , globBinds :: Map.Map String Type
  , nextVarI :: Integer
  , varToExp :: Map.Map String (Maybe Exp)
  }

insertGlobBind :: String -> Type -> TypecheckM ()
insertGlobBind id t = do
  modify (\state -> state{globBinds = globBinds state & Map.insert id t})

insertGlobBinds :: [(String, Type)] -> TypecheckM ()
insertGlobBinds = mapM_ (uncurry insertGlobBind)

emptyTypeStore :: TypeStore
emptyTypeStore = TypeStore{types = Map.empty, globBinds = Map.empty, nextVarI = 0, constructorToType = Map.empty, varToExp = Map.empty}

type TypecheckM = ReaderT TypeEnv (StateT TypeStore (ExceptT String IO))

runTypecheckM :: TypecheckM a -> TypeStore -> ExceptT String IO (a, TypeStore)
runTypecheckM f store = do
  runStateT (runReaderT f emptyTEnv) store

runTypecheckDecLst :: [Dec] -> TypeStore -> ExceptT String IO TypeStore
runTypecheckDecLst decs store = do
  (_, store') <- runTypecheckM (typecheckDecLst decs) store
  return store'

freshVar :: Maybe Exp -> TypecheckM Type
freshVar exp = do
  varI <- gets nextVarI
  let name = "'__a" ++ show varI
  modify (\store -> store{nextVarI = varI + 1, varToExp = varToExp store & Map.insert name exp})
  return $ RVar name

refreshVars :: Type -> TypecheckM Type
refreshVars t = do
  let a = collectVars t
  b <- mapM (const $ freshVar Nothing) a
  return $ t & substituteTypes (zip a b)

typecheckCon :: Con -> TypecheckM Type
typecheckCon x = case x of
  CInt _ _ -> return $ RTerm [] "int"
  CString _ _ -> return $ RTerm [] "string"
  CUnit _ -> return $ RTerm [] "unit"

typecheckExp :: Exp -> TypecheckM (Type, [(Type, Type)])
typecheckExp x = case x of
  ECon _ con -> do
    conT <- typecheckCon con
    return (conT, [])
  EObjCon p (IdCap idcap) -> typecheckExp (EId p (Id idcap))
  EId _ (Id id) -> do
    tBound <- asks (\env -> binds env & Map.lookup id)
    -- available <- asks (\env -> binds env & Map.keys & intercalate ",")
    case tBound of
      Just t -> do
        t' <- refreshVars t
        return (t', [])
      Nothing -> do
        tFree <- asks (\env -> freeBinds env & Map.lookup id)
        case tFree of
          Just t -> return (t, [])
          Nothing -> throwError $ "undefined symbol '" ++ id ++ "'"
  ETup _ exp exps -> do
    t <- freshVar $ Just x
    ts <- mapM typecheckExp (exp : exps)
    return (t, (t, RTerm (map fst ts) "__tuple") : concatMap snd ts)
  EApp _ exp1 exp2 -> do
    rett <- freshVar $ Just x
    (fnt, fndeps) <- typecheckExp exp1
    (argt, argdeps) <- typecheckExp exp2
    return (rett, (fnt, RTerm [argt, rett] "__fn") : fndeps ++ argdeps)
  ELet _ letbinds exp -> do
    expT <- freshVar $ Just x
    valTs <- mapM (const $ freshVar Nothing) letbinds
    let valBinds = zip (map letBindId letbinds) valTs & mapMaybe (\(a, b) -> (,b) <$> a)
    bindsAcc <-
      local
        (insertFreeBinds valBinds)
        ( foldlM
            ( \acc (letbind, bindT) -> do
                (valT, valAcc) <- typecheckExp (letBindExp letbind)
                return $ (valT, bindT) : valAcc ++ acc
            )
            []
            (zip letbinds valTs)
        )
    (innerT, innerAcc) <- local (insertFreeBinds valBinds) (typecheckExp exp)
    return (expT, (expT, innerT) : innerAcc ++ bindsAcc)
  ECase _ exp ecasebinds -> do
    (argT, argdeps) <- typecheckExp exp
    retT <- freshVar $ Just x
    p <- mapM typecheckECaseBind ecasebinds
    let (patTs, expTs, deps) = unzip3 p
    let makeEqDeps = map (,argT) patTs ++ map (,retT) expTs
    return (retT, makeEqDeps ++ argdeps ++ concat deps)
  EFn _ [Id id] retExp -> do
    fnT <- freshVar $ Just x
    argT <- freshVar $ Just x
    (retT, retdeps) <- local (insertFreeBind id argT) (typecheckExp retExp)
    return (fnT, (fnT, RTerm [argT, retT] "__fn") : retdeps)
  _ -> error "unexpected expression"

typecheckECaseBind :: ECaseBind -> TypecheckM (Type, Type, [(Type, Type)])
typecheckECaseBind x = case x of
  ECBJust _ pat exp -> do
    (patT, patBinds, patDeps) <- typecheckPat pat
    (expT, expDeps) <- local (insertFreeBinds $ Map.toList patBinds) (typecheckExp exp)
    return (patT, expT, patDeps ++ expDeps)

type PatBinds = Map.Map String Type

-- result type, values bound by the pattern match, constraints
typecheckPat :: Pat -> TypecheckM (Type, PatBinds, [(Type, Type)])
typecheckPat x = case x of
  PCon _ con -> do
    conT <- typecheckCon con
    return (conT, Map.empty, [])
  PId _ (Id id) -> do
    t <- freshVar Nothing
    return (t, Map.singleton id t, [])
  PWild _ -> do
    t <- freshVar Nothing
    return (t, Map.empty, [])
  PTup _ pat pats -> do
    p' <- mapM typecheckPat (pat : pats)
    let (elemTs, binds, deps) = unzip3 p'
    v <- freshVar Nothing
    binds' <- concatPatBinds binds
    return (v, binds', (v, RTerm elemTs "__tuple") : concat deps)
  PObjCon _ (IdCap idcap) -> typecheckPatObj idcap Nothing
  PObj _ (IdCap idcap) pat -> typecheckPatObj idcap (Just pat)
  _ -> error "unexpected pattern"

typecheckPatObj :: String -> Maybe Pat -> TypecheckM (Type, PatBinds, [(Type, Type)])
typecheckPatObj idcap usedArgPat = do
  v <- freshVar Nothing
  constrT <- gets (\state -> globBinds state & Map.lookup idcap)
  case (constrT, usedArgPat) of
    (Just (RTerm _ "__fn"), Just provArgPat) -> do
      constrT' <- refreshVars (fromJust constrT)
      let (RTerm [expectedArgT, retT] "__fn") = constrT'
      (provArgT, argbinds, argdeps) <- typecheckPat provArgPat
      return (v, argbinds, (v, retT) : (expectedArgT, provArgT) : argdeps)
    (Just (RTerm _ "__fn"), Nothing) -> throwError $ "constructor '" ++ idcap ++ "' requires an argument"
    (Just retT, Nothing) -> do
      retT' <- refreshVars retT
      return (v, Map.empty, [(v, retT')])
    (Just _, Just _) -> throwError $ "constructor '" ++ idcap ++ "' doesn't accept an argument"
    (Nothing, _) -> throwError $ "undefined variant '" ++ idcap ++ "'"

concatPatBinds :: [PatBinds] -> TypecheckM PatBinds
concatPatBinds ms = do
  let keys = concatMap Map.keys ms
  when (length keys /= length (nub keys)) $ throwError "duplicate bindings in a pattern"
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
typecheckDec x = case x of
  DLet _ letbinds -> typecheckDLet letbinds
  DType _ typbinds -> typecheckDType typbinds
  DExn _ exnbinds -> do
    mapM_
      ( \exnbind -> do
          let constrT = typToType <$> exnBindTyp exnbind
          insertConstructor (exnBindId exnbind) constrT [] "__exn"
      )
      exnbinds
  DOpen _ idcaps -> mapM_ (typecheckDOpen . fromIdCap) idcaps

typecheckDLet :: [LetBind] -> TypecheckM ()
typecheckDLet letbinds = do
  let ids = map letBindId letbinds
  valTs <- mapM (const $ freshVar Nothing) letbinds
  let valBinds = zip ids valTs & concatMaybesFst
  gBinds <- gets globBinds
  -- liftIO $ putStrLn $ "loading: " ++ show ids
  eqs <-
    local
      (\env -> env{binds = gBinds, freeBinds = Map.fromList valBinds})
      ( foldlM
          ( \acc (letbind, bindT) -> do
              (valT, valAcc) <- typecheckExp (letBindExp letbind)
              return $ (valT, bindT) : valAcc ++ acc
          )
          []
          (zip letbinds valTs)
      )
  vte <- gets varToExp
  -- liftIO $ putStrLn $ "eqs: " ++ (map (\(a, b) -> show a ++ " == " ++ show b) eqs & intercalate "\n")
  -- liftIO $
  --   putStrLn $
  --     "vars: "
  --       ++ ( concatMap collectVars (map fst eqs ++ map snd eqs) & nub
  --             & map
  --               ( \x -> show x ++ " from " ++ maybe "?" printTree (vte Map.! x)
  --               )
  --             & intercalate "\n"
  --          )
  case unifyAll eqs of
    Left s -> throwError s
    Right subst -> do
      let newValTs = valTs & map (simplifyVars . substituteTypes subst)
      insertGlobBinds $ zip ids newValTs & concatMaybesFst
      return ()

typecheckDType :: [TypBind] -> TypecheckM ()
typecheckDType typbinds = do
  mapM_
    ( \(TBJust _ typlst (Id typeId) dtags) -> do
        typeVars <- typsOfTypLst typlst & mapM getParamVar

        mapM_
          ( \dtag -> do
              let constrT = typToType <$> dtagTyp dtag
              insertConstructor (dtagId dtag) constrT typeVars typeId
          )
          dtags
    )
    typbinds
 where
  getParamVar :: Typ -> TypecheckM String
  getParamVar (TIdVar _ (IdVar x)) = return x
  getParamVar _ = throwError "a type can be parametrized only by type variables"

typecheckDOpen :: String -> TypecheckM ()
typecheckDOpen id = do
  source <- liftIO $ try (readFile ("src/" ++ id ++ ".ml"))
  case (source :: Either IOError String) of
    Left exn -> throwError $ "can't load module '" ++ id ++ "': " ++ show exn
    Right s -> case stringToDecs s of
      Left exn -> throwError $ "syntax error in module '" ++ id ++ "': " ++ exn
      Right decs -> typecheckDecLst decs

-- Inserts a global function or value constructing an object. Does not allow redefinitions.
insertConstructor :: String -> Maybe Type -> [String] -> String -> TypecheckM ()
insertConstructor constrId t typeVars typeId = do
  assertDoesntExist
  let typeVars' = map RVar typeVars
  let constrT = case t of
        Nothing -> RTerm typeVars' typeId
        Just x -> RTerm [x, RTerm typeVars' typeId] "__fn"
  insertGlobBind constrId constrT
  modify (\state -> state{constructorToType = constructorToType state & Map.insert constrId (t, typeId)})
 where
  assertDoesntExist :: TypecheckM ()
  assertDoesntExist = do
    does <- gets (\s -> globBinds s & Map.lookup constrId & isJust)
    when does $ throwError $ "redefinition of constructor '" ++ typeId ++ "'"

typecheckDecLst :: [Dec] -> TypecheckM ()
typecheckDecLst = mapM_ typecheckDec

insertBuiltinType :: String -> String -> TypecheckM ()
insertBuiltinType name spec = do
  let t = typToType $ fromRight (error $ "syntax error in builtin type: " ++ show spec) (pTyp $ myLexer spec)
  insertGlobBind name t
  return ()
