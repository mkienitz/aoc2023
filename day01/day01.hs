module Main (main) where

import Data.Char (isNumber)
import Data.List.Extra (isPrefixOf, tails)

solve :: (String -> String) -> String -> Int
solve preprocess = sum . map (read . sequence [head, last] . preprocess) . lines

p1 :: String -> Int
p1 = solve (filter isNumber)

p2 :: String -> Int
p2 = solve (concatMap findDig . tails)
  where
    findDig str =
      [ dig
        | (tok, dig) <-
            zip
              ( ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
                  ++ map show [1 .. 9]
              )
              (cycle ['1' .. '9']),
          tok `isPrefixOf` str
      ]

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2]
