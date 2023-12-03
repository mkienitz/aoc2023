module Main (main) where

import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.List.Extra (groupOn, partition)

-- Types :) --
type Coords = (Int, Int)

type Symbol = (Coords, Char)

type Digits = [Symbol]

type Schematic = [[Symbol]]

-- Parse :) --
parseInput :: String -> Schematic
parseInput = zipWith (\rIdx row -> zipWith (\c cIdx -> ((rIdx, cIdx), c)) row [0 ..]) [0 ..] . lines

-- Logic :) --
neighbors :: Coords -> [Coords]
neighbors (rIdx, cIdx) =
  let offsets = [-1, 0, 1]
   in [(rIdx + rOff, cIdx + cOff) | rOff <- offsets, cOff <- offsets]

partSchem :: (Char -> Bool) -> Schematic -> ([Digits], [Coords])
partSchem predicate =
  second (map fst . filter (predicate . snd) . concat) -- filter others and get coords
    . partition (any (isDigit . snd)) -- split numbers and others
    . concatMap (groupOn (isDigit . snd)) -- group adjacent digits

p1 :: Schematic -> Int
p1 schematic = sum . map (read . map snd) . filter hasSpecialNeighbor $ digits
  where
    (digits, specials) = partSchem (\c -> not (isDigit c || c == '.')) schematic
    hasSpecialNeighbor :: Digits -> Bool
    hasSpecialNeighbor = any ((`elem` concatMap neighbors specials) . fst)

p2 :: Schematic -> Int
p2 schematic = sum . map product . filter ((== 2) . length) . map touchedDigits $ stars
  where
    (digits, stars) = partSchem (== '*') schematic
    touchedDigits :: Coords -> [Int]
    touchedDigits star = map (read . map snd) . filter (any ((`elem` neighbors star) . fst)) $ digits

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
