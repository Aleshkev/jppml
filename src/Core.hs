module Core where

import Control.Monad.Cont (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Monad.State (gets)
import Eval (BuiltinVal, EvalM, EvalState (typeState), FnVal (FnVal), Ptr, Val (VFn, VInt, VObjCon, VString), anonSet, boolToVal, emptyEvalEnv, printVal, ptrGet, valToBool)
import Infer (niceShowType)
import qualified Typecheck

makeFn :: (Ptr -> EvalM Ptr) -> Val
makeFn f = VFn $ FnVal emptyEvalEnv f

makeBinFn :: (Ptr -> Ptr -> EvalM Ptr) -> Val
makeBinFn f = makeFn (anonSet . makeFn . f)

makeTernFn :: (Ptr -> Ptr -> Ptr -> EvalM Ptr) -> Val
makeTernFn f = makeFn (\ptr1 -> anonSet $ makeFn (anonSet . makeFn . f ptr1))

makeValBinFn :: (Val -> Val -> EvalM Val) -> Val
makeValBinFn f =
  makeBinFn
    ( \ptr1 ptr2 -> do
        [val1, val2] <- mapM ptrGet [ptr1, ptr2]
        ret <- f val1 val2
        anonSet ret
    )

makeIntBinFn :: (Integer -> Integer -> Integer) -> Val
makeIntBinFn op = makeValBinFn (\(VInt a) (VInt b) -> return $ VInt $ op a b)

makeRelFn :: (Integer -> Integer -> Bool) -> Val
makeRelFn op = makeValBinFn (\(VInt a) (VInt b) -> return $ boolToVal $ op a b)

builtinVals :: [BuiltinVal]
builtinVals =
  [
    ( "print"
    , "'a -> unit"
    , makeFn
        ( \argptr -> do
            s <- printVal argptr
            liftIO $ putStr s
            anonSet (VObjCon "__Unit")
        )
    )
  ,
    ( "to_string"
    , "'a -> string"
    , makeFn
        ( \argptr -> do
            s <- printVal argptr
            anonSet (VString s)
        )
    )
  ,
    ( "raise"
    , "'a -> 'b"
    , makeFn
        ( \argptr -> do
            s <- printVal argptr
            throwError s
        )
    )
  , ("__cat", "string -> string -> string", makeValBinFn (\(VString a) (VString b) -> return $ VString $ a ++ b))
  , ("__add", "int -> int -> int", makeIntBinFn (+))
  , ("__sub", "int -> int -> int", makeIntBinFn (-))
  , ("__mul", "int -> int -> int", makeIntBinFn (*))
  , ("__div", "int -> int -> int", makeIntBinFn div)
  , ("__eq", "int -> int -> bool", makeRelFn (==))
  , ("__ne", "int -> int -> bool", makeRelFn (/=))
  , ("__lt", "int -> int -> bool", makeRelFn (<))
  , ("__le", "int -> int -> bool", makeRelFn (<=))
  , ("__gt", "int -> int -> bool", makeRelFn (>))
  , ("__ge", "int -> int -> bool", makeRelFn (>=))
  ,
    ( "__if"
    , "bool -> (unit -> 'a) -> (unit -> 'a) -> 'a"
    , makeTernFn
        ( \ptr1 ptr2 ptr3 -> do
            [arg1, arg2, arg3] <- mapM ptrGet [ptr1, ptr2, ptr3]
            let VFn (FnVal env f) = if valToBool arg1 then arg2 else arg3
            u <- anonSet (VObjCon "__Unit")
            local (const env) (f u)
        )
    )
  ]