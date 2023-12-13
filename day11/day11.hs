module Main (main) where

import Data.List (transpose)

type Coords = (Int, Int)

type Task = [Coords]

parseInput :: String -> Task
parseInput =
  map fst
    . filter ((== '#') . snd)
    . concat
    . zipWith (\ridx line -> zipWith (\cidx c -> ((ridx, cidx), c)) [0 ..] line) [0 ..]
    . expanded
    . lines

expanded :: [String] -> [String]
expanded = transpose . concatMap expandLine . transpose . concatMap expandLine
  where
    expandLine line
      | all (== '.') line = [line, line]
      | otherwise = [line]

p1 :: Task -> Int
p1 coords = (`div` 2) . sum . map (uncurry manhattan) . filter (uncurry (/=)) $ (,) <$> coords <*> coords
  where
    manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

main :: IO ()
main = readFile "input.txt" >>= print . p1 . parseInput
