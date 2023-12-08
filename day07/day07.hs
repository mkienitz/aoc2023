module Main (main) where

import Control.Arrow ((&&&))
import Data.Either.Extra (fromRight')
import Data.List (foldl', group, nub, sort, sortBy)
import Data.List.Extra (maximumOn)
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- Types
type Card = Char

type Bid = Int

type CardOrder = [Card]

type Hand = [Card]

type Task = [(Hand, Bid)]

-- Parser

type Parser = Parsec Void String

taskP :: Parser Task
taskP = some ((,) <$> some alphaNumChar <* hspace <*> decimal <* newline) <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

-- Logic
compareHands :: [Card] -> (Hand -> Hand) -> Hand -> Hand -> Ordering
compareHands cardOrder pre h1 h2 = case compare (handType $ pre h1) (handType $ pre h2) of
  EQ -> compare (tieBreak h1) (tieBreak h2)
  x -> x
  where
    tieBreak = map cardStrength
      where
        cardStrength :: Card -> Int
        cardStrength c = M.fromList (zip cardOrder [0 ..]) M.! c

handType :: Hand -> Int
handType hand = snd . head . dropWhile (not . fst) $ zip preds (reverse [0 :: Int .. 6])
  where
    preds = sequence [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPair, onePair, highCard] hand
    nOfSize :: Int -> Int -> (Hand -> Bool)
    nOfSize n s hand = n == length [length mult | mult <- group $ sort hand, length mult == s]
    fiveOfAKind hand = 1 == length (nub hand)
    fourOfAKind = nOfSize 1 4
    fullHouse = uncurry (&&) . (threeOfAKind &&& onePair)
    threeOfAKind = nOfSize 1 3
    twoPair = nOfSize 2 2
    onePair = nOfSize 1 2
    highCard hand = length hand == length (nub hand)

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
