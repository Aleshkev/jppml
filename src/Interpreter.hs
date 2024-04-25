module Interpreter where

import LexSyntax
import ParSyntax

import AbsSyntax
import Control.Monad.Except (Except, ExceptT, runExceptT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Function (const, flip, (&))
import Data.List (elem, isPrefixOf, length, reverse, sort, unzip)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (Ord)
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.OldList (intercalate)
import GHC.Show (Show)
import GHC.Stack (HasCallStack)
import Preprocess (transTree)
import PrintSyntax (printTree)
import Util (ansiDefault, ansiRed)
import Prelude (Bool (False, True), Either (..), Eq ((/=), (==)), Foldable (foldl), IO, Int, Integer, Integral (div), Monad (return), Num ((*), (-)), Ord ((<), (<=), (>), (>=)), Show, String, all, error, fail, foldr, fst, map, putStr, putStrLn, readFile, show, snd, uncurry, zip, ($), (+), (++), (.), (/))

data FnVal = FnVal Env (Ptr -> Ev Ptr)
instance Show FnVal where
  show = const "<fn>"

data Val
  = VRec
  | VInt Integer
  | VString String
  | VFn FnVal
  | VBuiltinFn String
  | VTuple [Ptr]
  | VObjCon String
  | VObj String Ptr
  deriving (Show)

printVal :: Ptr -> Ev String
printVal ptr = printVal' [] ptr
 where
  printVal' :: [Ptr] -> Ptr -> Ev String
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
          VBuiltinFn _ -> return "<__fn>"
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

data Store = Store
  { vals :: Map.Map Ptr Val
  , nextPtr :: Ptr
  , globBinds :: Map.Map String Ptr
  }
  deriving (Show)

emptyStore :: Store
emptyStore = Store{vals = Map.empty, nextPtr = Ptr 0, globBinds = Map.empty}

newtype Env = Env
  { binds :: Map.Map String Ptr
  }
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env{binds = Map.empty}

type Ev = ReaderT Env (StateT Store (ExceptT String IO))


redError :: HasCallStack => String -> a
redError s = error $ ansiRed ++ s ++ ansiDefault

incPtr :: Ptr -> Ptr
incPtr (Ptr x) = Ptr (x + 1)

alloc :: () -> Ev Ptr
alloc () = do
  ptr <- gets nextPtr
  modify (\store -> store{nextPtr = incPtr ptr})
  return ptr

ptrGet :: Ptr -> Ev Val
ptrGet ptr = do
  val <- gets vals
  case val & Map.lookup ptr of
    Nothing -> redError $ "internal: null pointer '" ++ show ptr ++ "'"
    Just VRec -> error "value not constructed yet"
    Just x -> return x

ptrSet :: Ptr -> Val -> Ev ()
ptrSet ptr val = do
  modify (\store -> store{vals = vals store & Map.insert ptr val})

insertBind :: String -> Ptr -> Env -> Env
insertBind name ptr env =
  env{binds = binds env & Map.insert name ptr}

globBindSet :: String -> Ptr -> Ev ()
globBindSet name ptr = do
  modify (\store -> store{globBinds = globBinds store & Map.insert name ptr})

anonSet :: Val -> Ev Ptr
anonSet val = do
  ptr <- alloc ()
  ptrSet ptr val
  return ptr

evalCon :: Con -> Val
evalCon x = case x of
  CInt _ integer -> VInt integer
  CString _ string -> VString string
  CUnit _ -> VObjCon "__Unit"

evalToVal :: Exp -> Ev Val
evalToVal exp = do
  ptr <- evalExp exp
  ptrGet ptr

evalExp :: HasCallStack => Exp -> Ev Ptr
evalExp exp = case exp of
  ECon _ con ->
    anonSet (evalCon con)
  EObjCon _ (IdCap idcap) -> do
    evalExp (EId Nothing (Id idcap))
  EId _ (Id id) -> do
    asks
      ( \env -> case binds env & Map.lookup id of
          Just x -> x
          Nothing -> redError $ "internal: undefined symbol '" ++ id ++ "', available: " ++ (binds env & Map.keys & sort & intercalate ", ")
      )
  ETup _ exp exps -> do
    ptrs <- mapM evalExp (exp : exps)
    anonSet (VTuple ptrs)
  EApp _ fnexp argexp -> do
    argptr <- evalExp argexp
    -- argval <- ptrGet argptr
    fnval <- evalToVal fnexp
    case fnval of
      VFn (FnVal env f) -> local (const env) (f argptr)
      VBuiltinFn "__print" -> do
        s <- printVal argptr
        liftIO $ putStr s
        anonSet (VObjCon "__Unit")
      VBuiltinFn "__raise" -> do
        s <- printVal argptr
        fail s
      _ -> error "internal: illegal application"
  EIf _ condexp iftexp iffexp -> do
    condval <- evalToVal condexp
    let exp = case condval of VObjCon "True" -> iftexp; _ -> iffexp
    evalExp exp
  ELet _ [LBJust _ (Id xname) yexp] zexp -> do
    xptr <- alloc ()
    ptrSet xptr VRec
    local
      (insertBind xname xptr)
      ( do
          yval <- evalToVal yexp
          ptrSet xptr yval
          evalExp zexp
      )
  EMul _ exp1 exp2 -> evalIntOp exp1 exp2 (*)
  EDiv _ exp1 exp2 -> evalIntOp exp1 exp2 div
  EAdd _ exp1 exp2 -> evalIntOp exp1 exp2 (+)
  ESub _ exp1 exp2 -> evalIntOp exp1 exp2 (-)
  ECat _ exp1 exp2 -> do
    [arg1, arg2] <- mapM evalToVal [exp1, exp2]
    let (VString a) = arg1
    let (VString b) = arg2
    anonSet (VString (a ++ b))
  ERel _ exp1 erelop exp2 -> do
    arg1 <- evalToVal exp1
    arg2 <- evalToVal exp2
    let (VInt a) = arg1
    let (VInt b) = arg2
    let f = case erelop of
          EREq _ -> (==)
          ERNe _ -> (/=)
          ERLt _ -> (<)
          ERLe _ -> (<=)
          ERGt _ -> (>)
          ERGe _ -> (>=)
    anonSet (VObjCon (if f a b then "True" else "False"))
  ECase _ exp ecasebinds -> do
    ptr <- evalExp exp
    let f cases = case cases of
          [] -> fail "Match_failure"
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
matchPat :: Pat -> Ptr -> Ev (Maybe (Map.Map String Ptr))
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

evalIntOp :: Exp -> Exp -> (Integer -> Integer -> Integer) -> Ev Ptr
evalIntOp exp1 exp2 f = do
  arg1 <- evalToVal exp1
  arg2 <- evalToVal exp2
  let (VInt a) = arg1
  let (VInt b) = arg2
  anonSet (VInt (f a b))

evalDec :: Dec -> Ev ()
evalDec dec = case dec of
  DLet p [LBJust _ (Id xname) yexp] -> do
    xptr <- alloc ()
    ptrSet xptr VRec
    globBindSet xname xptr
    gbinds <- gets globBinds
    local
      (\env -> env{binds = gbinds})
      ( do
          yval <- evalToVal yexp
          ptrSet xptr yval
      )
  DType _ typbinds -> do
    mapM_
      ( \(TBJust _ _ _ dtags) -> do
          mapM_
            ( \dtag -> do
                let (id, val) = case dtag of
                      DTCon _ (IdCap id) -> (id, VObjCon id)
                      DTArg _ (IdCap id) _ -> (id, VFn (FnVal emptyEnv (anonSet . VObj id)))
                ptr <- anonSet val
                globBindSet id ptr
            )
            dtags
      )
      typbinds
  DExn _ exnbinds -> do
    mapM_
      ( \exnbind -> do
          let (id, val) = case exnbind of
                EBCon _ (IdCap id) -> (id, VObjCon id)
                EBArg _ (IdCap id) _ -> (id, VFn (FnVal emptyEnv (anonSet . VObj id)))
          ptr <- anonSet val
          globBindSet id ptr
      )
      exnbinds
  DOpen _ idcaps -> do
    mapM_
      ( \(IdCap id) -> do
          s <- liftIO $ readFile ("src/" ++ id ++ ".ml")
          evalString s
      )
      idcaps
  _ -> error "not implemented"

runEv f = do
  let f' = do
        p <- anonSet (VBuiltinFn "__print")
        globBindSet "print" p
        evalDec $ DOpen Nothing [IdCap "Core"]
        f

  runExceptT (runStateT (runReaderT f' emptyEnv) emptyStore)

evalString :: String -> Ev ()
evalString s = do
  case (fmap transTree . pListDec . myLexer) s of
    Left x ->
      liftIO $ putStrLn $ ansiRed ++ "error: " ++ x ++ ansiDefault
    Right x ->
      evalDecLst x

evalDecLst :: [Dec] -> Ev ()
evalDecLst = mapM_ evalDec

printVars :: () -> Ev String
printVars () = do
  store <- get
  ss <-
    globBinds store & Map.toAscList
      & mapM
        ( \(name, ptr) -> do
            s <- printVal ptr
            return $ name ++ " = " ++ s
        )
  return $ "  globals: { " ++ intercalate ", " ss ++ " }"
