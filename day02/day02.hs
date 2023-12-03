module Main (main) where

import Data.Either (fromRight)
import Data.Function (on)
import Data.List (groupBy)
import Data.List.Extra (maximum, sortOn)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- Types :) --
data Color = Red | Green | Blue
  deriving (Eq, Show, Ord)

type Draw = (Int, Color)

type Round = [Draw]

type Game = (Int, [Round])

type Task = [Game]

type Parser = Parsec Void String

-- Parser :) --
colorP :: Parser Color
colorP =
  choice
    [ Red <$ string "red",
      Green <$ string "green",
      Blue <$ string "blue"
    ]

drawP :: Parser Draw
drawP = (,) <$> decimal <* char ' ' <*> colorP

roundP :: Parser Round
roundP = some (drawP <* optional (string ", "))

gameP :: Parser Game
gameP =
  (,)
    <$> (string "Game " *> decimal <* string ": ")
    <*> some (roundP <* optional (string "; "))

taskP :: Parser Task
taskP = some (gameP <* optional newline) <* eof

parseInput :: String -> Task
parseInput = fromRight [] . runParser taskP "input.txt"

-- Logic :) --
p1 :: Task -> Int
p1 = sum . map fst . filter (all (all isValidDraw) . snd)
  where
    isValidDraw (n, col) = case col of
      Red -> n <= 12
      Green -> n <= 13
      Blue -> n <= 14

p2 :: Task -> Int
p2 =
  sum
    . map
      ( product -- get power of cube set
          . map (maximum . map fst) -- get maximum count per color
          . groupBy ((==) `on` snd) -- group by color
          . sortOn snd -- make draws adjacent by color
          . concat -- join rounds
          . snd -- get rounds
      )

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
