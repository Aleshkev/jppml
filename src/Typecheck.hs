{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Typecheck where

import AbsSyntax
import Control.Exception (assert)
import Control.Monad (forM_, zipWithM)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError), MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (MonadReader (local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT (runStateT), gets, modify)
import Data.Either (fromRight)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Infer (Type (RTerm, RVar), collectVars, substituteTypes, unifyAll)
import ParSyntax (myLexer, pTyp)
import PrintSyntax (printTree)
import Util (ansiDefault, ansiRed)

newtype TypeDec = TypeDec (Map.Map String [Type])

data TypeEnv = TypeEnv
  { binds :: Map.Map String Type
  }

insertBind :: String -> Type -> TypeEnv -> TypeEnv
insertBind name typ env =
  env{binds = binds env & Map.insert name typ}

insertBinds :: Map.Map String Type -> TypeEnv -> TypeEnv
insertBinds t env = foldl (\acc (k, v) -> insertBind k v acc) env (Map.toList t)

emptyTEnv :: TypeEnv
emptyTEnv = TypeEnv{binds = Map.empty}

data TypeStore = TypeStore
  { --Constructors which this type has.
    types :: Map.Map String [String]
  , -- What type does a constructor require, and what type does it belong to. Type is in the form RTerm ('a, 'b) name
    constructorToType :: Map.Map String (Maybe Type, String)
  , globBinds :: Map.Map String Type
  , nextVarI :: Integer
  }

insertGlobBind :: String -> Type -> TypecheckM ()
insertGlobBind id t = do
  modify (\state -> state{globBinds = globBinds state & Map.insert id t})

emptyTypeStore :: TypeStore
emptyTypeStore = TypeStore{types = Map.empty, globBinds = Map.empty, nextVarI = 0, constructorToType = Map.empty}

-- insertGlobBind :: String -> Type -> TypeStore -> TypeStore
-- insertGlobBind name typ store =
--   store{globBinds = globBinds store & Map.insert name typ}

type TypecheckM = ReaderT TypeEnv (StateT TypeStore (ExceptT String IO))

runTypecheckM :: TypecheckM a -> TypeStore -> ExceptT String IO (a, TypeStore)
runTypecheckM f store = do
  runStateT (runReaderT f emptyTEnv) store

runTypecheckDecLst :: [Dec] -> TypeStore -> ExceptT String IO TypeStore
runTypecheckDecLst decs store = do
  (_, store') <- runTypecheckM (typecheckDecLst decs) store
  return store'

freshVar :: TypecheckM Type
freshVar = do
  varI <- gets nextVarI
  modify (\store -> store{nextVarI = varI + 1})
  return $ RVar ("'__a" ++ show varI)

refreshVars :: Type -> TypecheckM Type
refreshVars t = do
  subs <-
    collectVars t
      & mapM
        ( \v -> do
            v' <- freshVar
            return (v, v')
        )
  return $ substituteTypes subs t

typecheckCon :: Con -> TypecheckM Type
typecheckCon x = case x of
  CInt _ _ -> return $ RTerm [] "int"
  CString _ _ -> return $ RTerm [] "string"
  CUnit _ -> return $ RTerm [] "unit"

letBindId :: LetBind -> Maybe String
letBindId = \case
  LBJust _ (Id id) _ -> Just id
  LBAnon _ _ -> Nothing

typecheckExp :: Exp -> TypecheckM (Type, [(Type, Type)])
typecheckExp x = case x of
  ECon _ con -> do
    conT <- typecheckCon con
    return (conT, [])
  EObjCon p (IdCap idcap) -> typecheckExp (EId p (Id idcap))
  EId _ (Id id) -> do
    t <- asks (\env -> binds env & Map.lookup id)
    available <- asks (\env -> binds env & Map.keys & intercalate ",")
    case t of
      Just t -> do
        -- TODO: do this only for already resolved types!!
        -- t' <- refreshVars t
        return (t, [])
      Nothing -> throwError $ "undefined symbol '" ++ id ++ "'" ++ ", " ++ available
  ETup _ exp exps -> do
    t <- freshVar
    ts <- mapM typecheckExp (exp : exps)
    return (t, (t, RTerm (map fst ts) "__tuple") : concatMap snd ts)
  EApp _ exp1 exp2 -> do
    rett <- freshVar
    (fnt, fndeps) <- typecheckExp exp1
    (argt, argdeps) <- typecheckExp exp2
    return (rett, (fnt, RTerm [argt, rett] "__fn") : fndeps ++ argdeps)
  ELet _ letbinds exp -> do
    v <- freshVar
    boundVars <- mapM (const freshVar) letbinds

    deps <-
      local
        ( \env ->
            foldl
              ( \env (x, y) -> case x of
                  LBJust _ (Id id) _ -> do
                    env & insertBind id y
                  LBAnon _ _ -> env
              )
              env
              (zip letbinds boundVars)
        )
        ( do
            deps1 <-
              foldlM
                ( \acc (letbind, bindT) -> case letbind of
                    LBJust _ id exp -> do
                      (valT, valDeps) <- typecheckExp exp
                      return $ (valT, bindT) : valDeps ++ acc
                    LBAnon _ exp -> do
                      (_, valDeps) <- typecheckExp exp
                      return $ valDeps ++ acc
                )
                []
                (zip letbinds boundVars)
            (retT, retDeps) <- typecheckExp exp
            return $ (v, retT) : retDeps ++ deps1
        )
    return (v, deps)
  ECase _ exp ecasebinds -> do
    (argT, argdeps) <- typecheckExp exp
    retT <- freshVar
    p <- mapM typecheckECaseBind ecasebinds
    let (patTs, expTs, deps) = unzip3 p
    let makeEqDeps = map (,argT) patTs ++ map (,retT) expTs
    return (retT, makeEqDeps ++ argdeps ++ concat deps)
  EFn _ [Id id] retExp -> do
    fnT <- freshVar
    argT <- freshVar
    (retT, retdeps) <- local (insertBind id argT) (typecheckExp retExp)
    return (fnT, (fnT, RTerm [argT, retT] "__fn") : retdeps)
  _ -> error "unexpected expression"

typecheckECaseBind :: ECaseBind -> TypecheckM (Type, Type, [(Type, Type)])
typecheckECaseBind x = case x of
  ECBJust _ pat exp -> do
    (patT, patBinds, patDeps) <- typecheckPat pat
    (expT, expDeps) <- local (insertBinds patBinds) (typecheckExp exp)
    return (patT, expT, patDeps ++ expDeps)

-- result type, values bound by the pattern match, constraints
typecheckPat :: Pat -> TypecheckM (Type, Map.Map String Type, [(Type, Type)])
typecheckPat x = case x of
  PCon _ con -> do
    conT <- typecheckCon con
    return (conT, Map.empty, [])
  PId _ (Id id) -> do
    t <- freshVar
    return (t, Map.singleton id t, [])
  PWild _ -> do
    t <- freshVar
    return (t, Map.empty, [])
  PTup _ pat pats -> do
    p' <- mapM typecheckPat (pat : pats)
    let (elemTs, binds, deps) = unzip3 p'
    v <- freshVar
    binds' <- concatMaps binds
    return (v, binds', (v, RTerm elemTs "__tuple") : concat deps)
  PObjCon _ (IdCap idcap) -> typecheckPatObj idcap Nothing
  PObj _ (IdCap idcap) pat -> typecheckPatObj idcap (Just pat)
  _ -> error "unexpected pattern"

typecheckPatObj :: String -> Maybe Pat -> TypecheckM (Type, Map.Map String Type, [(Type, Type)])
typecheckPatObj idcap usedArgPat = do
  v <- freshVar
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

concatMaps :: [Map.Map String Type] -> TypecheckM (Map.Map String Type)
concatMaps =
  foldlM
    ( \acc m -> do
        forM_
          (Map.keys m)
          ( \x -> case acc & Map.lookup x of
              Just _ -> throwError $ "multiple bindings for '" ++ x ++ "' in a pattern"
              Nothing -> return ()
          )
        return $ acc `Map.union` m
    )
    Map.empty

typecheckTyp :: Typ -> Type
typecheckTyp x = case x of
  TIdVar _ (IdVar idvar) -> RVar idvar
  TId _ typlst (Id id) ->
    RTerm (typecheckTypLst typlst) id
  TTup _ typ ttupelems -> do
    let elemTs = map typecheckTyp (typ : map (\(TTupJust _ x) -> x) ttupelems)
    RTerm elemTs "__tuple"
  TFn _ typ1 typ2 -> do
    let ts = map typecheckTyp [typ1, typ2]
    RTerm ts "__fn"

typecheckTypLst :: TypLst -> [Type]
typecheckTypLst x = case x of
  TLEmpty _ -> []
  TLOne _ typ -> map typecheckTyp [typ]
  TLMany _ typ typs -> map typecheckTyp (typ : typs)

typecheckDec :: Dec -> TypecheckM ()
typecheckDec x = case x of
  DLet _ [letbind] -> do
    t <- freshVar
    exp <- case letbind of
      LBJust _ (Id id) exp -> do
        insertGlobBind id t
        return exp
      LBAnon _ exp -> return exp

    binds <- gets (\state -> globBinds state & Map.toList & map (\(a, b) -> a ++ ": " ++ show b) & intercalate ", ")
    liftIO $ putStrLn $ "binds: " ++ binds

    gBinds <- gets globBinds
    (retT, deps) <- local (\env -> env{binds = gBinds}) (typecheckExp exp)

    liftIO $ putStrLn $ "deps: " ++ show deps
    let subst = unifyAll deps
    _ <- case subst of
      Left s -> liftIO $ putStrLn $ ansiRed ++ s ++ ": " ++ printTree exp ++ ansiDefault
      Right subst -> do
        liftIO $ putStrLn $ "subst: " ++ show subst
        let newRetT = substituteTypes subst retT
        liftIO $ putStrLn $ "deduced: " ++ show newRetT
        case letbind of
          LBJust _ (Id id) _ -> do
            insertGlobBind id newRetT
          LBAnon _ _ -> return ()

    return ()
  DType _ typbinds ->
    mapM_
      ( \(TBJust _ typlst (Id typeId) dtags) -> do
          typeVars <-
            mapM
              ( \t -> case t of
                  TIdVar _ (IdVar x) -> return x
                  _ -> throwError $ "data type must be parametrized only by type variables"
              )
              (transTypLst typlst)

          mapM_
            ( \dtag -> do
                let (constrId, constrT) = case dtag of
                      DTCon _ (IdCap id) -> (id, Nothing)
                      DTArg _ (IdCap id) typ -> (id, Just (typecheckTyp typ))
                insertConstructor constrId constrT typeVars typeId
            )
            dtags
      )
      typbinds
  DExn _ exnbinds -> do
    mapM_
      ( \exnbind -> do
          let (id, constrT) = case exnbind of
                EBCon _ (IdCap id) -> (id, Nothing)
                EBArg _ (IdCap id) typ -> (id, Just (typecheckTyp typ))
          insertConstructor id constrT [] "__exn"
      )
      exnbinds
  DOpen _ idcaps -> return ()
  _ -> error "unexpected declaration"

insertConstructor :: String -> Maybe Type -> [String] -> String -> TypecheckM ()
insertConstructor constrId t typeVars typeId = do
  let typeVars' = map RVar typeVars
  let constrT = case t of
        Nothing -> RTerm typeVars' typeId
        Just x -> RTerm [x, RTerm typeVars' typeId] "__fn"
  insertGlobBind constrId constrT
  modify (\state -> state{constructorToType = constructorToType state & Map.insert constrId (t, typeId)})

transTypLst x = case x of
  TLEmpty _ -> []
  TLOne _ typ -> [typ]
  TLMany _ typ typs -> typ : typs

typecheckDecLst :: [Dec] -> TypecheckM ()
typecheckDecLst = mapM_ typecheckDec

insertBuiltinType :: String -> String -> TypecheckM ()
insertBuiltinType name spec = do
  let t = typecheckTyp $ fromRight (error $ "syntax error in builtin type: " ++ show spec) (pTyp $ myLexer spec)
  insertGlobBind name t
  return ()
