module Main where

import Interpreter (evalDecLst, Store (Store), Env (Env), printVars, Ev, runEv)
import System.IO (putStrLn)
import Util (ansiRed, ansiDefault)
import ParSyntax (myLexer, pListDec)
import Preprocess (transTree)
import Control.Monad.IO.Class (MonadIO(liftIO))


repl' :: () -> Ev ()
repl' () = do
  line <- liftIO getLine
  case line of
    ":q" -> return ()
    _ -> case (fmap transTree .  pListDec . myLexer) line of
      Left x -> do
        liftIO $ putStrLn $ ansiRed ++ "error: " ++ x ++ ansiDefault
        repl' ()
      Right x -> do 
        evalDecLst x
        -- putStrLn $ show env ++ " " ++ show scope
        vars <- printVars ()
        liftIO $ putStrLn vars
        repl' ()

repl :: () -> IO ()
repl () = do
  _ <- runEv (repl' ())
  return ()

main :: IO ()
main = do
  putStrLn $ "JPPML repl. Type :q to quit."
  repl ()
  return ()
