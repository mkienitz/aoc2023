module Main (main) where

import Data.Bifunctor (Bifunctor (second))
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Card = Card
  { winning :: S.Set Int,
    hand :: [Int]
  }
  deriving (Show)

type Task = [Card]

type Parser = Parsec Void String

cardP :: Parser Card
cardP =
  (\w h -> Card {winning = S.fromList w, hand = h})
    <$> ( (string "Card" <* space1 <* decimal <* char ':' <* space1)
            *> some (decimal <* space)
            <* (space *> char '|' <* space)
        )
    <*> some (decimal <* space)

taskP :: Parser Task
taskP = some (cardP <* optional newline) <* eof

parseInput :: String -> Task
parseInput = fromRight [] . runParser taskP "input.txt"

nWinning :: Card -> Int
nWinning (Card {winning, hand}) = length $ filter (`S.member` winning) hand

p1 :: Task -> Int
p1 = sum . map ((2 ^) . (+ (-1))) . filter (> 0) . map nWinning

p2 :: Task -> Int
p2 task = M.foldl' ((. snd) . (+)) 0 $ foldl' update cardMap (M.keys cardMap)
  where
    update map id = foldl' (flip (M.adjust (second (+ occs)))) map copyIndices
      where
        (card, occs) = map M.! id
        copyIndices = take (nWinning card) [id + 1 ..]
    cardMap = M.fromList $ zip [1 ..] (map (,1) task)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
