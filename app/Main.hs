module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Eval (evalDecLst, printVars, EvalState, runEvalDecLst, runInitEvalM, evalEvalM)
import ParSyntax (myLexer, pListDec)
import Preprocess (transTree)
import System.IO (putStrLn)
import Util (ansiDefault, ansiRed, ansiMagenta)
import Typecheck (typecheckDecLst)
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadIO (liftIO), runExceptT)
import AbsSyntax (Dec)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Set as Set
import Core (builtinVals)


inputLine :: [Dec] -> EvalState -> IO EvalState
inputLine s store = do
  res <- runExceptT $ runEvalDecLst s store
  case res of
    Left err -> do
      putStrLn $ ansiRed ++ "error: " ++ err ++ ansiDefault
      return store
    Right store' -> return store'


repl' :: EvalState -> IO ()
repl' store = do
  line <- liftIO getLine

  case line of
    ":q" -> return ()
    ":t" -> do
      (Right vars) <- runExceptT $ evalEvalM printVars store
      liftIO $ putStrLn $ ansiMagenta ++ (vars & intercalate "\n") ++ ansiDefault
      repl' store
    _ -> case (fmap transTree . pListDec . myLexer) line of
      Left x -> do
        liftIO $ putStrLn $ ansiRed ++ "error: " ++ x ++ ansiDefault
        repl' store
      Right x -> do
        store' <- inputLine x store

        (Right vars) <- runExceptT $ evalEvalM printVars store
        (Right vars') <- runExceptT $ evalEvalM printVars store'
        let newVars = (Set.fromList vars' Set.\\ Set.fromList vars) & Set.toAscList & intercalate "\n"
        liftIO $ putStrLn $ ansiMagenta ++ (if newVars /= "" then newVars else "  -") ++ ansiDefault 

        repl' store'

repl :: () -> IO ()
repl () = do
  maybeStore <- runExceptT $ runInitEvalM builtinVals
  case maybeStore of
    Left err -> do
      putStrLn $ ansiRed ++ "error while initialising: " ++ err ++ ansiDefault
    Right store -> do
      repl' store

main :: IO ()
main = do
  putStrLn $ "JPPML repl. Type :q to quit, :t to print bindings."
  repl ()
  return ()
