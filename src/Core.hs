module Core where

import Control.Monad (when)
import Control.Monad.Cont (liftIO)
import Control.Monad.Except (MonadError (catchError), throwError)
import Control.Monad.Reader (local)
import Eval (BuiltinVal, EvalM, FnVal (FnVal), Ptr, Val (VFn, VInt, VObj, VObjCon, VString), boolToVal, emptyEvalEnv, eqVal, newPtrWith, printVal, ptrGet, valToBool)
import Util (mapTupleM)

makeFn :: (Ptr -> EvalM Ptr) -> Val
makeFn f = VFn $ FnVal emptyEvalEnv f

makeBinFn :: (Ptr -> Ptr -> EvalM Ptr) -> Val
makeBinFn f = makeFn (newPtrWith . makeFn . f)

makeTernFn :: (Ptr -> Ptr -> Ptr -> EvalM Ptr) -> Val
makeTernFn f = makeFn (\ptr1 -> newPtrWith $ makeFn (newPtrWith . makeFn . f ptr1))

makeValBinFn :: (Val -> Val -> EvalM Val) -> Val
makeValBinFn f =
  makeBinFn (\ptr1 ptr2 -> mapTupleM ptrGet (ptr1, ptr2) >>= uncurry f >>= newPtrWith)

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
    ( "__try" -- Tries to execute the function and catches runtime errors. Returns "Some" with error message as string, or "None".
    , "(unit -> unit) -> string option"
    , makeFn
        ( \argptr -> do
            u <- newPtrWith (VObjCon "__Unit")
            VFn (FnVal env f) <- ptrGet argptr
            err <- catchError (local (const env) (f u >> return Nothing)) (return . Just)
            case err of
              Nothing -> newPtrWith $ VObjCon "None"
              Just x -> newPtrWith (VString x) >>= (newPtrWith . VObj "Some")
        )
    )
  ,
    ( "to_string"
    , "'a -> string"
    , makeFn (\argptr -> printVal argptr >>= (newPtrWith . VString))
    )
  ,
    ( "raise"
    , "'a -> 'b"
    , makeFn (\argptr -> printVal argptr >>= throwError)
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
    , makeBinFn (\a b -> eqVal a b >>= (newPtrWith . boolToVal))
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
