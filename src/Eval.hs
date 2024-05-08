module Eval where

import AbsSyntax (Con, Con' (CInt, CString, CUnit), Dec, Dec' (DExn, DLet, DOpen, DType), ECaseBind' (ECBJust), ExnBind, Exp, Exp' (EApp, ECase, ECon, EFn, EId, ELet, EObjCon, ETup), Id (Id), IdCap (IdCap), LetBind, Pat, Pat' (PCon, PId, PObj, PObjCon, PTup, PWild), TypBind, TypBind' (TBJust))
import AbsUtil (dtagHasArg, dtagId, exnBindHasArg, exnBindId, letBindExp, letBindId, printPosition, stringToDecs)
import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, throwError)
import Control.Monad.Reader (Functor (fmap), Monad ((>>=)), MonadIO (liftIO), MonadReader (ask, local), ReaderT (runReaderT), asks, mapM, mapM_)
import Control.Monad.State (MonadState (get), StateT (runStateT), gets, modify)
import Data.Function (const, (&))
import Data.List (elem, intercalate, zip)
import qualified Data.Map as Map
import Data.Maybe (Maybe (..), catMaybes, isJust, maybe)
import Data.Ord (Ord)
import Infer (niceShowType)
import PrintSyntax (printTree)
import Typecheck (TypeState, emptyTypeState, insertBuiltinType, runTypecheckM, typecheckDec, typecheckDecLst)
import qualified Typecheck
import Util (catMaybesFst, foldInserter)
import Prelude (Bool (False, True), Either (..), Eq ((==)), IO, Integer, Monad (return), Show, String, all, error, foldr, map, not, readFile, show, uncurry, ($), (&&), (+), (++), (.))

data Val
  = VRec -- This is a special value that represents a non-constructed value, between declaration and initialisation.
  | VInt Integer
  | VString String
  | VFn FnVal
  | VTuple [Ptr]
  | VObjCon String
  | VObj String Ptr
  deriving (Show)

-- FnVal is separated from VFn so that we can derive Show on Val.
data FnVal = FnVal EvalEnv (Ptr -> EvalM Ptr)
instance Show FnVal where
  show = const "<fn>"

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
  { binds :: Map.Map String Ptr -- Local bindings.
  }
  deriving (Show)

emptyEvalEnv :: EvalEnv
emptyEvalEnv = EvalEnv{binds = Map.empty}

type EvalM = ReaderT EvalEnv (StateT EvalState (ExceptT String IO))

runEvalM :: EvalM a -> EvalState -> IO (Either String (a, EvalState))
runEvalM f state =
  runExceptT $ runStateT (runReaderT f emptyEvalEnv) state

boolToVal :: Bool -> Val
boolToVal b = if b then VObjCon "True" else VObjCon "False"

valToBool :: Val -> Bool
valToBool v = case v of VObjCon "False" -> False; _ -> True

type BuiltinVal = (String, String, Val) -- identifier, type (to be parsed), value

incPtr :: Ptr -> Ptr
incPtr (Ptr x) = Ptr (x + 1)

newPtr :: EvalM Ptr
newPtr = do
  ptr <- gets nextPtr
  modify (\state -> state{nextPtr = incPtr ptr})
  return ptr

newPtrWith :: Val -> EvalM Ptr
newPtrWith val = do
  ptr <- newPtr
  ptrSet ptr val
  return ptr

ptrGet :: Ptr -> EvalM Val
ptrGet ptr = do
  val <- gets vals
  case val & Map.lookup ptr of
    Nothing -> throwError $ "internal: null pointer '" ++ show ptr ++ "'"
    Just VRec -> throwError "value not yet constructed"
    Just x -> return x

ptrSet :: Ptr -> Val -> EvalM ()
ptrSet ptr val =
  modify (\state -> state{vals = vals state & Map.insert ptr val})

insertBind :: String -> Ptr -> EvalEnv -> EvalEnv
insertBind name ptr env =
  env{binds = binds env & Map.insert name ptr}

insertBinds :: [(String, Ptr)] -> EvalEnv -> EvalEnv
insertBinds = foldInserter insertBind

insertGlobBind :: String -> Ptr -> EvalState -> EvalState
insertGlobBind name ptr state =
  state{globBinds = globBinds state & Map.insert name ptr}

insertGlobBinds :: [(String, Ptr)] -> EvalState -> EvalState
insertGlobBinds = foldInserter insertGlobBind

-- Compares two values by structure.
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

-- Pretty-prints a value. Detects cycles.
printVal :: Ptr -> EvalM String
printVal ptr = printVal' [] True ptr
 where
  printVal' :: [Ptr] -> Bool -> Ptr -> EvalM String
  printVal' vis spacesAllowed ptr = do
    let wrap s = if spacesAllowed then s else "(" ++ s ++ ")"
    if ptr `elem` vis
      then return "<cycle>"
      else do
        let vis' = ptr : vis
        let aux = printVal' vis'
        val <- ptrGet ptr
        case val of
          VRec -> error "non-constructed value"
          VInt x -> return $ show x
          VString x -> return $ show x
          VFn _ -> return "<fn>"
          VTuple ptrs -> do
            vals <- mapM (aux True) ptrs
            return $ "(" ++ intercalate ", " vals ++ ")"
          VObjCon "__Unit" -> return "()"
          VObjCon "__Empty" -> return "[]"
          VObjCon x -> return x
          VObj "__Cons" _ -> do
            ps <- collectList vis ptr
            f <- mapM (maybe (return "<cycle>") (aux True)) ps & fmap (intercalate ", ")
            return $ "[" ++ f ++ "]"
          VObj x p -> do
            e <- aux False p
            return $ wrap $ x ++ " " ++ e
  -- Collect elements of a list (necessary because list is recursively defined).
  -- Detects cycles and returns "Nothing" in the position where the cycle is.
  collectList :: [Ptr] -> Ptr -> EvalM [Maybe Ptr]
  collectList vis ptr = do
    if ptr `elem` vis
      then return [Nothing]
      else do
        let vis' = ptr : vis
        val <- ptrGet ptr
        case val of
          VObjCon "__Empty" -> return []
          VObj "__Cons" x -> do
            val <- ptrGet x
            let (VTuple [p, nextp]) = val
            ps <- collectList (p : vis') nextp
            return $ Just p : ps
          x -> error $ "unexpected value in a list: " ++ show x

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

evalCon :: Con -> Val
evalCon x = case x of
  CInt _ integer -> VInt integer
  CString _ string -> VString string
  CUnit _ -> VObjCon "__Unit"

evalLet :: [LetBind] -> Maybe Exp -> EvalM ([(String, Ptr)], Maybe Ptr)
evalLet letbinds innerExp = do
  let (ids, valExps) = (map letBindId letbinds, map letBindExp letbinds)
  valPtrs <- mapM (const newPtr) letbinds
  mapM_ (\ptr -> ptrSet ptr VRec) valPtrs
  let binds = zip ids valPtrs & catMaybesFst
  ptr <-
    local
      (insertBinds binds)
      ( do
          mapM_
            (\(exp, ptr) -> evalToVal exp >>= ptrSet ptr)
            (zip valExps valPtrs)
          case innerExp of
            Nothing -> return Nothing
            Just innerExp -> evalExp innerExp & fmap Just
      )
  return (binds, ptr)

evalExp :: Exp -> EvalM Ptr
evalExp exp = case exp of
  ECon _ con ->
    newPtrWith (evalCon con)
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
    newPtrWith (VTuple ptrs)
  EApp _ fnexp argexp -> do
    argptr <- evalExp argexp
    VFn (FnVal env f) <- evalToVal fnexp
    local (const env) (f argptr)
  ELet _ letbinds innerExp -> do
    (_, Just ptr) <- evalLet letbinds (Just innerExp)
    return ptr
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
    newPtrWith (VFn (FnVal env f))
  x -> error $ "internal: expression unexpected at the interpreter stage: " ++ printTree x

evalToVal :: Exp -> EvalM Val
evalToVal exp = evalExp exp >>= ptrGet

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
  gBinds <- gets globBinds
  (binds, Nothing) <- local (\env -> env {binds = gBinds}) (evalLet letbinds Nothing)
  modify (insertGlobBinds binds)

evalDType :: TypBind -> EvalM ()
evalDType (TBJust _ _ _ dtags) = do
  mapM_ (\dtag -> insertConstructor (dtagId dtag) (dtagHasArg dtag)) dtags

evalDExn :: ExnBind -> EvalM ()
evalDExn exnbind = do
  insertConstructor (exnBindId exnbind) (exnBindHasArg exnbind)

insertConstructor :: String -> Bool -> EvalM ()
insertConstructor constrId hasArg = do
  let val = if not hasArg then VObjCon constrId else VFn (FnVal emptyEvalEnv (newPtrWith . VObj constrId))
  ptr <- newPtrWith val
  modify (insertGlobBind constrId ptr)

evalOpen :: String -> EvalM ()
evalOpen id = liftIO (readFile ("src/" ++ id ++ ".ml")) >>= evalString

insertBuiltins :: [BuiltinVal] -> EvalM ()
insertBuiltins vals = do
  mapM_
    ( \(name, typeStr, val) -> do
        p <- newPtrWith val
        modify (insertGlobBind name p)
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