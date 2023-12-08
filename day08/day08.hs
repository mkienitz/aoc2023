module Main (main) where

import Data.Either.Extra (fromRight')
import Data.List (foldl1', scanl')
import Data.List.Utils (endswith)
import Data.Map qualified as M
import Data.Tuple.Extra (fst3)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Direction = L | R
  deriving (Show)

type Node = String

type Branch = (Node, Node, Node)

type Task = ([Direction], [Branch])

type Parser = Parsec Void String

directionsP :: Parser [Direction]
directionsP = some ((R <$ string "R") <|> (L <$ string "L")) <* newline

nodeP :: Parser Branch
nodeP =
  (,,)
    <$> some alphaNumChar
    <* string " = ("
    <*> some alphaNumChar
    <* string ", "
    <*> some alphaNumChar
    <* char ')'
    <* newline

taskP :: Parser Task
taskP = (,) <$> directionsP <* newline <*> some nodeP

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

solve :: Node -> (Node -> Bool) -> [Direction] -> [Branch] -> Int
solve start stopCond dirs branches =
  length
    . takeWhile (not . stopCond . fst3)
    $ scanl' goDir (lut M.! start) (cycle dirs)
  where
    lut = M.fromList [(n, b) | b@(n, _, _) <- branches]
    goDir (_, l, _) L = lut M.! l
    goDir (_, _, r) R = lut M.! r

p1 :: Task -> Int
p1 = uncurry $ solve "AAA" (== "ZZZ")

p2 :: Task -> Int
p2 (dirs, branches) =
  foldl1' lcm $
    [ solve n (endswith "Z") dirs branches
      | (n, _, _) <- branches,
        endswith "A" n
    ]

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
