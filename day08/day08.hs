module Main (main) where

import Control.DeepSeq (force)
import Data.Either.Extra (fromRight')
import Data.List (foldl1', sort)
import Data.List.Extra (sortOn)
import Data.List.Utils (endswith)
import Data.Map qualified as M
import Data.Tuple.Extra (fst3)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- Types

data Direction = L | R
  deriving (Show)

type Node = (String, String, String)

type Task = ([Direction], [Node])

type Parser = Parsec Void String

-- Parser
directionsP :: Parser [Direction]
directionsP = some ((R <$ string "R") <|> (L <$ string "L")) <* newline

nodeP :: Parser Node
nodeP = (,,) <$> some alphaNumChar <* string " = (" <*> some alphaNumChar <* string ", " <*> some alphaNumChar <* char ')' <* newline

taskP :: Parser Task
taskP = (,) <$> directionsP <* newline <*> some nodeP

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

-- Logic
solve :: [Node] -> [Direction] -> String -> (String -> Bool) -> Int
solve nodes dirs start stop =
  length
    . takeWhile (not . stop . fst3)
    . map fst
    $ iterate g (lut M.! start, cycle dirs)
  where
    lut :: M.Map String Node
    lut = M.fromList $ map (\node@(name, _, _) -> (name, node)) nodes
    g :: (Node, [Direction]) -> (Node, [Direction])
    g (node, dir : dirs) = (f node dir, dirs)
    f :: Node -> Direction -> Node
    f (n, l, r) L = lut M.! l
    f (n, l, r) R = lut M.! r

p1 :: Task -> Int
p1 (dirs, nodes) = solve nodes dirs "AAA" (== "ZZZ")

p2 :: Task -> Int
p2 (dirs, nodes) = foldl1' lcm $ map runLengths startNodes
  where
    startNodes = map fst3 $ filter (endswith "A" . fst3) nodes
    runLengths start = solve nodes dirs start (endswith "Z")

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
