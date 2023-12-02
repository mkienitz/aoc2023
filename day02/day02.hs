module Main (main) where

import Data.Either (fromRight)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- Types :) --
data Color = Red | Green | Blue
  deriving (Show, Eq)

type Draw = (Int, Color)

type Round = [Draw]

type Game = (Int, [Round])

type Task = [Game]

newtype Total = Total (Int, Int, Int)
  deriving (Eq)

instance Ord Total where
  (<=) (Total (r1, g1, b1)) (Total (r2, g2, b2)) = r1 <= r2 && g1 <= g2 && b1 <= b2

type Parser = Parsec Void String

-- Parser :) --
intP :: Parser Int
intP = read <$> some numberChar

colorP :: Parser Color
colorP =
  choice
    [ Red <$ string "red",
      Green <$ string "green",
      Blue <$ string "blue"
    ]

drawP :: Parser Draw
drawP = (,) <$> intP <* char ' ' <*> colorP

roundP :: Parser Round
roundP = some (drawP <* optional (string ", "))

gameP :: Parser Game
gameP = (,) <$> (string "Game " *> intP <* string ": ") <*> some (roundP <* optional (string "; "))

taskP :: Parser Task
taskP = some (gameP <* optional newline) <* eof

parseInput :: String -> Task
parseInput = fromRight [] . runParser taskP "input.txt"

-- Logic :) --
calcPerColor :: ([Int] -> a) -> [Draw] -> (a, a, a)
calcPerColor f draws = (apply Red, apply Green, apply Blue)
  where
    apply col = f . map fst . filter ((==) col . snd) $ draws

p1 :: Task -> Int
p1 = sum . map fst . filter (isValidGame (Total (12, 13, 14)))
  where
    isValidGame total = all (isValidRound total) . snd
    isValidRound total = (<= total) . Total . calcPerColor sum

p2 :: Task -> Int
p2 = sum . map toPower
  where
    toPower :: Game -> Int
    toPower = (\(r, g, b) -> r * g * b) . calcPerColor maximum . concat . snd

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
