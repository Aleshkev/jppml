-- This module is for testing only. Code is uglier than in main files.
module Main (main) where

import AbsUtil (stringToDecs)
import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Core (builtinVals)
import Data.Either (isRight, rights)
import Data.Function
import Data.List (intercalate, isInfixOf, isPrefixOf, sort)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Eval (EvalState (typeState), emptyEvalState, evalDecLst, evalString, insertBuiltins, printVars, runEvalM)
import ParSyntax
import Preprocess (transTree)
import PrintSyntax (printTree)
import System.Directory (getDirectoryContents, listDirectory)
import System.Exit (exitFailure)
import Typecheck (TypeState (globBinds), runTypecheckM, typecheckDecLst)
import Util

groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines lines =
  let (group, rest) = break null lines
   in group : case rest of
        [] -> []
        (_ : xs) -> groupLines xs

allEqual :: (Eq a) => [a] -> Bool
allEqual xs = case xs of
  [] -> True
  x : xs' -> all (== x) xs'

replace :: String -> String -> String -> String
replace a a' s = Text.replace (p a) (p a') (p s) & unp
 where
  p = Text.pack; unp = Text.unpack

doTest :: String -> (String -> IO (Either a b)) -> (a -> String) -> (b -> String) -> IO ()
doTest inputFile makeResult showLeft showRight = do
  putStrLn $ "suite: " ++ inputFile
  suite <- readFile inputFile
  let groups = suite & lines & filter (not . ("--" `isPrefixOf`)) & groupLines
  mapM_ testGroup groups
 where
  process line = line & replace "#" "" & makeResult

  printTree2 = either showLeft showRight

  isLineOk line tree = do
    let shouldBeOk = not $ "#" `isInfixOf` line
    let isOk = tree & isRight
    if shouldBeOk /= isOk
      then do
        putStrLn $ ansiRed ++ (if shouldBeOk then "This should be ok:" else "This shouldn't be ok:") ++ ansiDefault
        putStrLn $ ansiBrightWhite ++ "  " ++ line ++ ansiDefault
        putStrLn $ "    " ++ printTree2 tree
        return False
      else return True

  testGroup :: [String] -> IO Bool
  testGroup lines = do
    trees <- lines & mapM process
    let treesPrinted = trees & rights & map showRight
    let allMatch = treesPrinted & allEqual

    allOk <- zipWithM isLineOk lines trees & fmap and
    unless
      allMatch
      ( do
          putStrLn $ ansiRed ++ "Results don't match:" ++ ansiDefault
          zipWithM_
            ( \line tree -> do
                putStrLn $ ansiBrightWhite ++ "  " ++ line ++ ansiDefault
                putStrLn $ "    " ++ printTree2 tree
            )
            lines
            trees
      )
    return (allOk && allMatch)

getType :: String -> IO (Either String String)
getType s = do
  case (fmap transTree . pListDec . myLexer) s of
    Left x -> return $ Left x
    Right decs -> do
      -- liftIO $ runExceptT $ runTypecheckM (typecheckDOpen "Core") tState
      (Right (_, evalState)) <- runEvalM (insertBuiltins builtinVals) emptyEvalState
      let ts = typeState evalState
      typeRet <- liftIO $ runTypecheckM (typecheckDecLst decs) ts
      case typeRet of
        Left (err, _) -> return $ Left $ "type error: " ++ err
        Right (_, typeState) -> do
          let a = globBinds ts
          let b = globBinds typeState
          let c = b Map.\\ a
          return $ Right $ c & Map.toAscList & map (\(a, b) -> a ++ " : " ++ show b) & intercalate " ; "

getVal :: String -> IO (Either String String)
getVal s = do
  (Right (_, evalState)) <- runEvalM (insertBuiltins builtinVals) emptyEvalState
  (Right (vars, _)) <- runEvalM printVars evalState

  ret <- runEvalM (evalString s) evalState
  case ret of
    Left err -> return $ Left err
    Right (_, evalState') -> do
      (Right (vars', _)) <- runEvalM printVars evalState'
      return $ Right $ diffLines vars vars' & unlines

doLongTests :: IO ()
doLongTests = do
  ok <- listDirectory "good" & fmap sort
  mapM_
    ( ( \file -> do
          s <- readFile file
          v <- getVal s
          case v of
            Left err -> putStrLn $ ansiRed ++ "error when executing " ++ show file ++ ": " ++ err ++ ansiDefault
            Right _ -> putStrLn $ ansiGreen ++ "executed " ++ show file ++ ansiDefault
          return ()
      )
        . ("good/" ++)
    )
    ok

main :: IO ()
main = do
  doTest "test/test_grammar.txt" (return . pListDec . myLexer) id printTree
  doTest "test/test_preprocessor.txt" (return . stringToDecs) id printTree
  doTest "test/test_typecheck.txt" getType id id
  doTest "test/test_interpreter.txt" getVal id id
  doLongTests

  exitFailure
