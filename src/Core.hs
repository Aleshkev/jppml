module Core where

import Control.Monad (when)
import Control.Monad.Cont (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (local)
import Control.Monad.State (gets)
import Eval (BuiltinVal, EvalM, EvalState (typeState), FnVal (FnVal), Ptr, Val (VFn, VInt, VObjCon, VString), newPtrWith, boolToVal, emptyEvalEnv, eqVal, printVal, ptrGet, valToBool)
import Infer (niceShowType)
import qualified Typecheck

makeFn :: (Ptr -> EvalM Ptr) -> Val
makeFn f = VFn $ FnVal emptyEvalEnv f

makeBinFn :: (Ptr -> Ptr -> EvalM Ptr) -> Val
makeBinFn f = makeFn (newPtrWith . makeFn . f)

makeTernFn :: (Ptr -> Ptr -> Ptr -> EvalM Ptr) -> Val
makeTernFn f = makeFn (\ptr1 -> newPtrWith $ makeFn (newPtrWith . makeFn . f ptr1))

makeValBinFn :: (Val -> Val -> EvalM Val) -> Val
makeValBinFn f =
  makeBinFn
    ( \ptr1 ptr2 -> do
        [val1, val2] <- mapM ptrGet [ptr1, ptr2]
        ret <- f val1 val2
        newPtrWith ret
    )

makeIntBinFn :: (Integer -> Integer -> Integer) -> Val
makeIntBinFn op = makeValBinFn (\(VInt a) (VInt b) -> return $ VInt $ op a b)

makeRelFn :: (Integer -> Integer -> Bool) -> Val
makeRelFn op = makeValBinFn (\(VInt a) (VInt b) -> return $ boolToVal $ op a b)

builtinVals :: [BuiltinVal]
builtinVals =
  [
    ( "print_string"
    , "string -> unit"
    , makeFn
        ( \argptr -> do
            (VString s) <- ptrGet argptr
            liftIO $ putStr s
            newPtrWith (VObjCon "__Unit")
        )
    )
  ,
    ( "to_string"
    , "'a -> string"
    , makeFn
        ( \argptr -> do
            s <- printVal argptr
            newPtrWith (VString s)
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
  ,
    ( "__div"
    , "int -> int -> int"
    , makeValBinFn
        ( \(VInt a) (VInt b) -> do
            when (b == 0) $ throwError "Division_by_zero"
            return $ VInt $ a `div` b
        )
    )
  ,
    ( "__eq"
    , "'a -> 'a -> bool"
    , makeBinFn
        ( \a b -> do
            eq <- eqVal a b
            newPtrWith $ boolToVal eq
        )
    )
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
            u <- newPtrWith (VObjCon "__Unit")
            local (const env) (f u)
        )
    )
  ]
