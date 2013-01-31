module Main (main) where

import Control.Arrow ((***))

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Bits (xor)
import Data.Char (ord, chr, isLetter)
import Data.Function (on)
import Data.List (sortBy, groupBy, nub, foldl')

import Data.Hex (unhex)

type Key = IntMap Char

strXor :: String -> String -> String
strXor xs ys = map chr $ zipWith xor (map ord xs) (map ord ys)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

readInput :: FilePath -> IO (String, [String])
readInput path = do
  text <- fmap (filter (not . null) . lines) (readFile path)
  unhexed <- mapM unhex text
  return (last unhexed, init unhexed)

maybeSpaces :: String -> [String] -> [(Int, [Char])]
maybeSpaces x ys = candidates4
  where xors = [strXor x y | y <- ys]
        threshold = (length ys * 75) `div` 100
        candidates0 = concatMap (filter (isLetter . snd) . enumerate) xors
        candidates1 = sortBy (compare `on` fst) candidates0
        candidates2 = groupBy ((==) `on` fst) candidates1
        candidates3 = filter ((>=threshold) . length . snd) $ map k candidates2
          where k xs@(x:_) = (fst x, map snd xs)
        candidates4 = map (id *** nub) candidates3

guessKey' :: String -> [String] -> Key -> Key
guessKey' x ys = process
  where process key = foldl' k key ms
          where k key (i, (c, _)) = IntMap.insert i ckey key
                  where ckey = chr $ ord c `xor` ord ' '

                ms = merge (enumerate x) (maybeSpaces x ys)

        merge [] _ = []
        merge _ [] = []
        merge ((i, x):xs') ys@((j, y):ys')
          | j == i = (i, (x, y)) : merge xs' ys'
          | otherwise = merge xs' ys

guessKey :: [String] -> Key
guessKey xs = foldl' k IntMap.empty splits
  where n = length xs
        splits = [(head rs, ls ++ tail rs) | i <- [0..n-1],
                                             let (ls, rs) = splitAt i xs]

        k keys (x, xs) = guessKey' x xs keys

decode :: Key -> String -> String
decode key str = foldr ((:) . go) [] (zip [0..] str)
  where go (i, c) | Just ckey <- IntMap.lookup i key = chr $ ord c `xor` ord ckey
                  | otherwise                        = '_'

main :: IO ()
main = do
  (target, others) <- readInput "input.txt"
  print $ decode (guessKey (target:others)) target
