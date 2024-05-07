{-# LANGUAGE LambdaCase #-}

module Eval where

import ParSyntax

import AbsSyntax
import AbsUtil (dtagHasArg, dtagId, exnBindHasArg, exnBindId, letBindExp, letBindId, printPosition, stringToDecs)
import Control.Exception (try)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (const, (&))
import Data.List (elem, zip)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (Ord)
import GHC.OldList (intercalate)
import GHC.Show (Show)
import GHC.Stack (HasCallStack)
import Infer (niceShowType)
import Preprocess (transTree)
import Typecheck (TypeState, emptyTypeState, insertBuiltinType, runTypecheckM, typecheckDecLst, typecheckDec)
import qualified Typecheck
import Util (ansiDefault, ansiRed, catMaybesFst)
import Prelude (Bool (False, True), Either (..), Eq ((==)), IO, IOError, Integer, Monad (return), String, all, error, foldr, map, not, putStrLn, readFile, show, uncurry, ($), (&&), (+), (++), (.))

data FnVal = FnVal EvalEnv (Ptr -> EvalM Ptr)
instance Show FnVal where
  show = const "<fn>"

data Val
  = VRec
  | VInt Integer
  | VString String
  | VFn FnVal
  | VTuple [Ptr]
  | VObjCon String
  | VObj String Ptr
  deriving (Show)

boolToVal :: Bool -> Val
boolToVal b = if b then VObjCon "True" else VObjCon "False"

valToBool :: Val -> Bool
valToBool v = case v of VObjCon "True" -> True; _ -> False

type BuiltinVal = (String, String, Val) -- identifier, type (to be parsed), value

eqVal :: Ptr -> Ptr -> EvalM Bool
eqVal aPtr bPtr = do
  [a, b] <- mapM ptrGet [aPtr, bPtr]
  case (a, b) of
    (VInt x, VInt y) -> return $ x == y
    (VString x, VString y) -> return $ x == y
    (VTuple xs, VTuple ys) -> do
      bs <- zipWithM eqVal xs ys
      return $ all (== True) bs
    (VObjCon x, VObjCon y) -> return $ x == y
    (VObj x x', VObj y y') -> do
      b <- eqVal x' y'
      return $ x == y && b
    _ -> return False

printVal :: Ptr -> EvalM String
printVal ptr = printVal' [] ptr
 where
  printVal' :: [Ptr] -> Ptr -> EvalM String
  printVal' vis ptr = do
    if ptr `elem` vis
      then return "<cycle>"
      else do
        let vis' = ptr : vis
        let aux = printVal' vis'
        val <- ptrGet ptr
        case val of
          VRec -> return "<rec>"
          VInt x -> return $ show x
          VString x -> return $ show x
          VFn _ -> return "<fn>"
          VTuple ptrs -> do
            vals <- mapM aux ptrs
            return $ "(" ++ intercalate ", " vals ++ ")"
          VObjCon "__Unit" -> return "()"
          VObjCon "__Empty" -> return "[]"
          VObjCon x -> return $ "(" ++ x ++ ")"
          VObj "__Cons" x -> do
            xval <- ptrGet x
            let (VTuple [p, nextp]) = xval
            e <- aux p
            es <- printVal' (p : vis') nextp
            return $ e ++ " :: " ++ es
          VObj x p -> do
            e <- aux p
            return $ "(" ++ x ++ " " ++ e ++ ")"

newtype Ptr = Ptr Integer deriving (Eq, Ord, Show)

data EvalState = EvalState
  { vals :: Map.Map Ptr Val
  , nextPtr :: Ptr
  , globBinds :: Map.Map String Ptr
  , typeState :: TypeState
  }

emptyEvalState :: EvalState
emptyEvalState = EvalState{vals = Map.empty, nextPtr = Ptr 0, globBinds = Map.empty, typeState = emptyTypeState}

newtype EvalEnv = EvalEnv
  { binds :: Map.Map String Ptr
  }
  deriving (Show)

emptyEvalEnv :: EvalEnv
emptyEvalEnv = EvalEnv{binds = Map.empty}

type EvalM = ReaderT EvalEnv (StateT EvalState (ExceptT String IO))

runEvalM :: EvalM a -> EvalState -> IO (Either String (a, EvalState))
runEvalM f state = do
  runExceptT $ runStateT (runReaderT f emptyEvalEnv) state

incPtr :: Ptr -> Ptr
incPtr (Ptr x) = Ptr (x + 1)

alloc :: EvalM Ptr
alloc = do
  ptr <- gets nextPtr
  modify (\state -> state{nextPtr = incPtr ptr})
  return ptr

ptrGet :: Ptr -> EvalM Val
ptrGet ptr = do
  val <- gets vals
  case val & Map.lookup ptr of
    Nothing -> throwError $ "internal: null pointer '" ++ show ptr ++ "'"
    Just VRec -> throwError "value not yet constructed"
    Just x -> return x

ptrSet :: Ptr -> Val -> EvalM ()
ptrSet ptr val = do
  modify (\state -> state{vals = vals state & Map.insert ptr val})

insertBind :: String -> Ptr -> EvalEnv -> EvalEnv
insertBind name ptr env =
  env{binds = binds env & Map.insert name ptr}

globBindSet :: String -> Ptr -> EvalM ()
globBindSet name ptr = do
  modify (\state -> state{globBinds = globBinds state & Map.insert name ptr})

anonSet :: Val -> EvalM Ptr
anonSet val = do
  ptr <- alloc
  ptrSet ptr val
  return ptr

evalCon :: Con -> Val
evalCon x = case x of
  CInt _ integer -> VInt integer
  CString _ string -> VString string
  CUnit _ -> VObjCon "__Unit"

evalToVal :: Exp -> EvalM Val
evalToVal exp = do
  ptr <- evalExp exp
  ptrGet ptr

evalExp :: HasCallStack => Exp -> EvalM Ptr
evalExp exp = case exp of
  ECon _ con ->
    anonSet (evalCon con)
  EObjCon _ (IdCap idcap) -> do
    evalExp (EId Nothing (Id idcap))
  EId _ (Id id) -> do
    asks
      ( \env -> case binds env & Map.lookup id of
          Just x -> x
          Nothing -> error $ "internal: undefined symbol '" ++ id ++ "'"
      )
  ETup _ exp exps -> do
    ptrs <- mapM evalExp (exp : exps)
    anonSet (VTuple ptrs)
  EApp _ fnexp argexp -> do
    argptr <- evalExp argexp
    VFn (FnVal env f) <- evalToVal fnexp
    local (const env) (f argptr)
  ELet _ [LBJust _ (Id xname) yexp] zexp -> do
    -- TODO: multiple letbinds
    -- TODO: move things in common with global letbind
    xptr <- alloc
    ptrSet xptr VRec
    local
      (insertBind xname xptr)
      ( do
          yval <- evalToVal yexp
          ptrSet xptr yval
          evalExp zexp
      )
  ECase _ exp ecasebinds -> do
    ptr <- evalExp exp
    let f cases = case cases of
          [] -> throwError "Match_failure"
          (pat, exp) : cases' -> do
            match <- matchPat pat ptr
            case match of
              Just binds -> do
                local (\env -> foldr (uncurry insertBind) env (Map.toList binds)) (evalExp exp)
              Nothing -> f cases'
    f $ map (\(ECBJust _ pat exp) -> (pat, exp)) ecasebinds
  EFn _ [Id argname] exp -> do
    env <- ask
    let f argptr = local (insertBind argname argptr) (evalExp exp)
    anonSet (VFn (FnVal env f))
  _ ->
    error "internal: expression unexpected at the interpreter stage"

-- Checks if the value matches the pattern. If yes, returns Map with captured variables.
matchPat :: Pat -> Ptr -> EvalM (Maybe (Map.Map String Ptr))
matchPat pat ptr = do
  val <- ptrGet ptr
  case (pat, val) of
    (PCon _ (CInt _ lx), VInt rx) | lx == rx -> return $ Just Map.empty
    (PCon _ (CString _ lx), VString rx) | lx == rx -> return $ Just Map.empty
    (PId _ (Id id), _) -> return $ Just $ Map.fromList [(id, ptr)]
    (PWild _, _) -> return $ Just Map.empty
    (PTup _ pat pats, VTuple rptrs) -> do
      deps <- zipWithM matchPat (pat : pats) rptrs
      if all isJust deps
        then return $ Just $ foldr Map.union Map.empty (catMaybes deps)
        else return Nothing
    (PObjCon _ (IdCap lidcap), VObjCon ridcap) | lidcap == ridcap -> return $ Just Map.empty
    (PObj _ (IdCap lidcap) pat, VObj ridcap rptr) | lidcap == ridcap -> matchPat pat rptr
    _ -> return Nothing

evalDec :: Dec -> EvalM ()
evalDec dec = case dec of
  DLet _ letbinds -> evalDLet letbinds
  DType _ typbinds -> mapM_ evalDType typbinds
  DExn _ exnbinds -> mapM_ evalDExn exnbinds
  DOpen _ idcaps -> mapM_ (\(IdCap id) -> evalOpen id) idcaps

evalDLet :: [LetBind] -> EvalM ()
evalDLet letbinds = do
  let ids = map letBindId letbinds
  valPtrs <- mapM (const alloc) letbinds
  mapM_ (`ptrSet` VRec) valPtrs
  mapM_ (uncurry globBindSet) (zip ids valPtrs & catMaybesFst)
  gBinds <- gets globBinds
  local
    (\env -> env{binds = gBinds})
    ( mapM_
        ( \(letbind, ptr) -> do
            yval <- evalToVal $ letBindExp letbind
            ptrSet ptr yval
        )
        (zip letbinds valPtrs)
    )

evalDType :: TypBind -> EvalM ()
evalDType (TBJust _ _ _ dtags) = do
  mapM_ (\dtag -> insertConstructor (dtagId dtag) (dtagHasArg dtag)) dtags

evalDExn :: ExnBind -> EvalM ()
evalDExn exnbind = do
  insertConstructor (exnBindId exnbind) (exnBindHasArg exnbind)

insertConstructor :: String -> Bool -> EvalM ()
insertConstructor constrId hasArg = do
  let val = if not hasArg then VObjCon constrId else VFn (FnVal emptyEvalEnv (anonSet . VObj constrId))
  ptr <- anonSet val
  globBindSet constrId ptr

evalOpen :: String -> EvalM ()
evalOpen id = liftIO (readFile ("src/" ++ id ++ ".ml")) >>= evalString

insertBuiltins :: [BuiltinVal] -> EvalM ()
insertBuiltins vals = do
  mapM_
    ( \(name, typeStr, val) -> do
        p <- anonSet val
        globBindSet name p
        tState <- gets typeState
        (Right (_, typeState')) <- liftIO $ runTypecheckM (insertBuiltinType name typeStr) tState
        modify (\state -> do state{typeState = typeState'})
        return ()
    )
    vals

  tState <- gets typeState
  coreOk <- liftIO $ runTypecheckM (typecheckDec (DOpen Nothing [IdCap "Core"])) tState
  case coreOk of
    Left err -> throwError $ "when loading 'Core':" ++ show err
    Right _ -> return ()

  evalOpen "Core"

evalString :: String -> EvalM ()
evalString s = do
  case stringToDecs s of
    Left x -> throwError x
    Right x -> evalDecLst x

evalDecLst :: [Dec] -> EvalM ()
evalDecLst decs = do
  typeState <- gets typeState
  typeRet <- liftIO $ runTypecheckM (typecheckDecLst decs) typeState
  case typeRet of
    Left (msg, src) -> throwError $ "type error at " ++ printPosition src ++ ": " ++ msg
    Right (_, typeState') -> do
      modify (\state -> state{typeState = typeState'})
      mapM_ evalDec decs

printVars :: EvalM [String]
printVars = do
  state <- get
  typeState <- gets typeState
  let m = Typecheck.globBinds typeState
  globBinds state & Map.toAscList
    & mapM
      ( \(name, ptr) -> do
          s <- printVal ptr
          return $ name ++ " : " ++ niceShowType (m Map.! name) ++ " = " ++ s
      )
