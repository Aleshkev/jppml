module Main (main) where

import Control.Monad
import Data.Bool (bool)
import Data.Either (isLeft, isRight, rights)
import Data.Function
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import ParSyntax
import PrintSyntax (printTree)
import System.Exit (exitFailure)
import Util
import Preprocess (transTree)

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

doPreprocessor line =
  line & myLexer & pListDec



doTest :: String -> (String -> Either a b) -> (a -> String) -> (b -> String) -> IO ()
doTest inputFile makeResult showLeft showRight = do
  suite <- readFile inputFile
  let groups = suite & lines & filter (not . ("--" `isPrefixOf`)) & groupLines
  mapM_ testGroup groups
 where
  process line = line & replace "#" "" & makeResult

  printTree2 = either showLeft showRight

  isLineOk line tree = do
    let shouldBeOk = not $ "#" `isInfixOf` line
    let isOk = tree & isRight
    if shouldBeOk /= isOk then do
      putStrLn $ ansiRed ++ (if shouldBeOk then "This should be ok:" else "This shouldn't be ok:") ++ ansiDefault
      putStrLn $ ansiBrightWhite ++ "  " ++ line ++ ansiDefault
      putStrLn $ "    " ++ printTree2 tree
      return False
    else return True

  testGroup :: [String] -> IO Bool
  testGroup lines = do
    let trees = lines & map process
    let treesPrinted = trees & rights & map showRight
    let allMatch = treesPrinted & allEqual

    allOk <- zipWithM isLineOk lines trees & fmap and
    if not allMatch
      then do
        putStrLn $ ansiRed ++ "Results don't match:" ++ ansiDefault
        zipWithM_ (\line tree -> do
          putStrLn $ ansiBrightWhite ++ "  " ++ line ++ ansiDefault
          putStrLn $ "    " ++ printTree2 tree) lines trees
      else do return ()
    return (allOk && allMatch)

main :: IO ()
main = do
  doTest "test/test_grammar.txt" (pListDec . myLexer) id printTree
  doTest "test/test_preprocessor.txt" (fmap transTree .  pListDec . myLexer) id printTree
  exitFailure
