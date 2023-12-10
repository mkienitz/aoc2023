module Main (main) where

import Data.Either.Extra (fromRight')
import Data.List (elemIndex, foldl', group, sort, sortBy)
import Data.List.Extra (maximumOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Card = Char

type Bid = Int

type CardOrder = [Card]

type Hand = [Card]

type Task = [(Hand, Bid)]

type Parser = Parsec Void String

taskP :: Parser Task
taskP = some ((,) <$> some alphaNumChar <* hspace <*> decimal <* newline) <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

compareHands :: [Card] -> (Hand -> Hand) -> Hand -> Hand -> Ordering
compareHands cardOrder pre h1 h2 = case compare (handType $ pre h1) (handType $ pre h2) of
  EQ -> compare (tieBreak h1) (tieBreak h2)
  x -> x
  where
    tieBreak = map (fromMaybe 0 . flip elemIndex cardOrder)

handType :: Hand -> Int
handType hand = case sort . map length . group $ sort hand of
  [5] -> 6
  [1, 4] -> 5
  [2, 3] -> 4
  [1, 1, 3] -> 3
  [1, 2, 2] -> 2
  [1, 1, 1, 2] -> 1
  _ -> 0

solve :: CardOrder -> (Hand -> Hand) -> Task -> Int
solve cardOrder pre =
  sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (\(h1, _) (h2, _) -> compareHands cardOrder pre h1 h2)

p1 :: Task -> Int
p1 = solve "23456789TJQKA" id

p2 :: Task -> Int
p2 = solve "J23456789TQKA" bestPermut
  where
    bestPermut :: Hand -> Hand
    bestPermut = maximumOn handType . permuts
      where
        permuts :: Hand -> [Hand]
        permuts hand = foldl' step [[]] (map candidateCards hand)
          where
            candidateCards :: Card -> [Card]
            candidateCards 'J' = "AKQT98765432J"
            candidateCards c = [c]
            step :: [Hand] -> [Card] -> [Hand]
            step hands cards = concatMap (\h -> map (: h) cards) hands

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
