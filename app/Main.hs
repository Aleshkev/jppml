module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Eval (printVars, EvalState, runEvalM, evalString, insertBuiltins, emptyEvalState)
import System.IO (putStrLn)
import Util (ansiDefault, ansiRed, ansiMagenta, putStrLnErr)
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Set as Set
import Core (builtinVals)
import Control.Monad (when)
import System.Environment (getArgs)


redError :: String -> IO ()
redError s = putStrLnErr $ ansiRed ++ "error: " ++ s ++ ansiDefault 


-- Print variables that changed value. Compares by string representation so not perfect but good enough.
printChangedValues :: EvalState -> EvalState -> IO ()
printChangedValues state state' = do
    (Right (vars, _)) <- runEvalM printVars state
    (Right (vars', _)) <- runEvalM printVars state'
    let newVars = (Set.fromList vars' Set.\\ Set.fromList vars) & Set.toAscList & intercalate "\n"
    when (newVars /= "") $ liftIO $ putStrLn $ ansiMagenta ++ newVars ++ ansiDefault 


repl' :: EvalState -> IO ()
repl' state = do
  line <- liftIO getLine

  case line of
    ":q" -> return ()
    ":t" -> do
      (Right (vars, _)) <- runEvalM printVars state
      liftIO $ putStrLn $ ansiMagenta ++ (vars & intercalate "\n") ++ ansiDefault
      repl' state
    _ -> do
      result <- runEvalM (evalString line) state
      case result of
        Left err -> do
          redError err
          repl' state
        Right (_, state') -> do
          printChangedValues state state'
          repl' state'

repl :: () -> IO ()
repl () = do
  result <- runEvalM (insertBuiltins builtinVals) emptyEvalState 
  case result of
    Left err -> do
      redError $ "during initialization: " ++ err
    Right (_, state) -> do
      putStrLn $ ansiMagenta ++ "JPPML repl. Type :q to quit, :t to print bindings." ++ ansiDefault
      repl' state

doFile :: String -> IO ()
doFile s = do
  code <- readFile s
  (Right (_, state)) <- runEvalM (insertBuiltins builtinVals) emptyEvalState
  result <- runEvalM (evalString code) state
  case result of
    Left err -> do
      putStrLnErr $ "error: " ++ err
    Right _ -> do
      return ()
  
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl ()
    [s] -> doFile s
    _ -> error "too many arguments"
