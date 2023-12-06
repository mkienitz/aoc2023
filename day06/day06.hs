module Main (main) where

import Data.Either.Extra (fromRight')
import Data.Number.CReal
import Data.Tuple.Extra (both)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Time = Int

type Distance = Int

type Race = (Time, Distance)

type Task = ([Time], [Distance])

type Parser = Parsec Void String

taskP :: Parser Task
taskP =
  (,)
    <$> (string "Time:" <* space *> (decimal `sepBy` some (char ' ')) <* newline)
    <*> (string "Distance:" <* space *> (decimal `sepBy` some (char ' ')) <* newline)
    <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

-- solveNaive :: Race -> Int
-- solveNaive (t, d) = length . filter (> d) $ [bt * (t - bt) | bt <- [0 .. t]]

solve :: Race -> Int
solve (t, d) = sol (-) (-) - sol (+) (+) + 1
  where
    (tD :: CReal, dD) = both fromIntegral (t, d)
    sol pmC pmI =
      let s = ceiling ((-tD) `pmC` sqrt (tD ^ 2 - 4 * dD) / (-2))
       in if s * (t - s) > d then s else s `pmI` 1

p1 :: Task -> Int
p1 = product . map solve . uncurry zip

p2 :: Task -> Int
p2 = solve . both (read . concatMap show)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
