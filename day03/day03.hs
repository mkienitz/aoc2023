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
parseInput = zipWith (\x row -> zipWith (\c y -> ((x, y), c)) row [0 ..]) [0 ..] . lines

-- Logic :) --
neighbors :: Coords -> [Coords]
neighbors (x, y) = [(x + xOff, y + yOff) | xOff <- [-1, 0, 1], yOff <- [-1, 0, 1]]

partSchem :: (Char -> Bool) -> Schematic -> ([Digits], [Coords])
partSchem predicate =
  second (map fst . filter (predicate . snd) . concat)
    . partition (any (isDigit . snd))
    . concatMap (groupOn (isDigit . snd))

p1 :: Schematic -> Int
p1 schem = sum . map (read . map snd) $ filter touchesSpecials digits
  where
    (digits, specials) = partSchem (\c -> not (isDigit c || c == '.')) schem
    touchesSpecials = any ((`elem` concatMap neighbors specials) . fst)

p2 :: Schematic -> Int
p2 schem = sum . map product . filter ((== 2) . length) $ map touchedNumbers stars
  where
    (digits, stars) = partSchem (== '*') schem
    touchedNumbers star =
      map (read . map snd) $ filter (any ((`elem` neighbors star) . fst)) digits

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
