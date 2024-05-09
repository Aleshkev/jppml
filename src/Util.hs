module Util where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.List (nub, isInfixOf)
import System.IO (stderr, hPutStrLn)

ansiDefault :: String
ansiDefault = "\ESC[0m"

ansiRed :: String
ansiRed = "\ESC[31m"

ansiGreen :: String
ansiGreen = "\ESC[32m"

ansiYellow :: String
ansiYellow = "\ESC[33m"

ansiBlue :: String
ansiBlue = "\ESC[34m"

ansiMagenta :: String
ansiMagenta = "\ESC[35m"

ansiCyan :: String
ansiCyan = "\ESC[36m"

ansiBrightWhite :: String
ansiBrightWhite = "\ESC[97m"

fromRight :: Either a b -> b
fromRight e = case e of
  Left _ -> error "fromRight"
  Right x -> x

-- a, b, ..., z, then aa, ab, ..., zz, aaa, aab, and so on.
cellNames :: [String]
cellNames =
  enumFrom 1 & concatMap f
 where
  az = map (: []) ['a' .. 'z']
  f :: Integer -> [String]
  f 1 = az
  f n = concatMap (\x -> map (x ++) (f $ n - 1)) az

catMaybesFst :: [(Maybe a, b)] -> [(a, b)]
catMaybesFst = mapMaybe (\(a, b) -> (,b) <$> a)

diffLines :: [String] -> [String] -> [String]
diffLines old new = Set.fromList new Set.\\ Set.fromList old & Set.toAscList

existDuplicates :: Eq a => [a] -> Bool
existDuplicates l = length (nub l) /= length l

isReserved :: String -> Bool
isReserved s = "__" `isInfixOf` s || "'__" `isInfixOf` s

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

foldInserter :: (String -> a -> b -> b) -> ([(String, a)] -> b -> b)
foldInserter f t env = foldl (\acc (k, v) -> f k v acc) env t

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

mapTupleM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
mapTupleM f (a1, a2) = do
  b1 <- f a1
  b2 <- f a2
  return (b1, b2)
