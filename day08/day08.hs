module Main (main) where

import Data.Either.Extra (fromRight')
import Data.Map qualified as M
import Data.Tuple.Extra (fst3)
import Data.Void (Void)
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
p1 :: Task -> Int
p1 (dirs, nodes) = length . takeWhile (not . stop) . map fst $ iterate g (startNode, cycle dirs)
  where
    lut :: M.Map String Node
    lut = M.fromList $ map (\node@(name, l, r) -> (name, node)) nodes

    startNode = lut M.! "AAA"

    stop :: Node -> Bool
    stop ("ZZZ", _, _) = True
    stop _ = False

    g :: (Node, [Direction]) -> (Node, [Direction])
    g (node, dir : dirs) = (f node dir, dirs)

    f :: Node -> Direction -> Node
    f (n, l, r) L = lut M.! l
    f (n, l, r) R = lut M.! r

main :: IO ()
main = readFile "input.txt" >>= print . p1 . parseInput
