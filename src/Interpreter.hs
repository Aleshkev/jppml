module Interpreter where

import LexSyntax
import ParSyntax

import AbsSyntax
import Data.Function ((&))
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Ord (Ord)
import GHC.Show (Show)
import Prelude (Either (..), Eq, Foldable (foldl), IO, Int, Integer, Monad (return), Ord, Show, String, error, fail, map, show, ($), (+), (++))

data Val
  = VNonConstructed
  | VInt Integer
  | VString String
  | VFn Scope String Exp
  | VTuple [Ptr]
  | VObjCon String
  | VObj String Ptr

newtype Ptr = Ptr Integer deriving (Eq, Ord)

data Env = Env
  { vals :: Map.Map Ptr Val
  , nextPtr :: Ptr
  }
data Scope = Scope
  { binds :: Map.Map String Ptr
  }

incPtr :: Ptr -> Ptr
incPtr (Ptr x) = Ptr (x + 1)

alloc :: Env -> (Env, Ptr)
alloc env = do
  let ptr = nextPtr env
  (env{nextPtr = incPtr ptr}, ptr)


ptrGet :: Env -> Ptr -> Val
ptrGet env ptr =
  case vals env ! ptr of
    VNonConstructed -> error "Recursive value"
    x -> x

ptrSet :: Env -> Ptr -> Val -> Env
ptrSet env ptr val =
  env{vals = vals env & Map.insert ptr val}

bindSet :: Scope -> String -> Ptr -> Scope
bindSet scope name ptr =
  scope{binds = binds scope & Map.insert name ptr}

anonSet :: Env -> Val -> (Env, Ptr)
anonSet env val = do
  let (env', ptr) = alloc env
  let env'' = ptrSet env' ptr val
  (env'', ptr)


evalCon :: Con -> Val
evalCon x = case x of
  CInt _ integer -> VInt integer
  CString _ string -> VString string
  CUnit _ -> VObjCon "__Unit"

evalExp :: Exp -> (Env, Scope) -> (Env, Ptr)
evalExp exp (env, scope) = case exp of
  ECon _ con -> anonSet env (evalCon con)
  EObjCon _ (IdCap idcap) -> error "not implemented"
  EId _ (Id id) -> (env, binds scope ! id)
  ETup p exp exps -> anonSet env (VTuple [])
  EApp p exp1 exp2 -> do
    let (env', fnptr) = evalExp exp1 (env, scope)
    let (VFn fnscope fnargname fnexp) = ptrGet env' fnptr
    let (env'', argptr) = evalExp exp2 (env', scope)
    let fnscope' = bindSet fnscope fnargname argptr
    evalExp fnexp (env'', fnscope')
  ELet _ [LBJust _ (Id name) exp1] exp2 -> do
    let (env', ptr) = alloc env
    let env'' = ptrSet env' ptr VNonConstructed
    let scope' = bindSet scope name ptr
    let (env''', valptr) = evalExp exp1 (env'', scope)
    let env'''' = ptrSet env''' ptr (ptrGet env valptr)
    evalExp exp2 (env'''', scope')
  ECase _ exp ecasebinds -> error "not implemented"
  EFn _ [Id argname] exp -> anonSet env (VFn scope argname exp)
  _ -> error "expression unexpected at the interpreter stage"


transTree :: String -> ParSyntax.Err [Dec]
transTree s =
  pListDec (myLexer s)
