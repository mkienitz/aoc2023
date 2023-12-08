module Main (main) where

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.Either.Extra (fromRight')
import Data.List (find, group, nub, sort)
import Data.List.Extra (groupOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

newtype Card = Card Char
  deriving (Show, Eq)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare c1 c2 = compare (strength c1) (strength c2)
    where
      strength :: Card -> Int
      strength (Card c) = M.fromList (zip (['2' .. '9'] ++ ['T', 'J', 'Q', 'K', 'A']) [0 ..]) M.! c

newtype Hand = Hand [Card]
  deriving (Show, Eq)

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1 h2 = case compare (handType h1) (handType h2) of
    EQ -> tieBreaker h1 h2
    x -> x
    where
      tieBreaker (Hand cards1) (Hand cards2) = maybe EQ (uncurry compare) (find (uncurry (/=)) (zip cards1 cards2))

handType :: Hand -> Int
handType hand = snd . head . dropWhile (not . fst) $ zip x (reverse [0 :: Int .. 6])
  where
    x = sequence [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPair, onePair, highCard] hand

nOfSize :: Int -> Int -> Hand -> Bool
nOfSize n s (Hand cards) = n == length [length mult | mult <- group $ sort cards, length mult == s]

fiveOfAKind :: Hand -> Bool
fiveOfAKind (Hand cards) = 1 == length (nub cards)

fourOfAKind :: Hand -> Bool
fourOfAKind = nOfSize 1 4

fullHouse :: Hand -> Bool
fullHouse = uncurry (&&) . (threeOfAKind &&& onePair)

threeOfAKind :: Hand -> Bool
threeOfAKind = nOfSize 1 3

twoPair :: Hand -> Bool
twoPair = nOfSize 2 2

onePair :: Hand -> Bool
onePair = nOfSize 1 2

highCard :: Hand -> Bool
highCard (Hand cards) = length cards == length (nub cards)

type Bid = Int

type Task = [(Hand, Bid)]

type Parser = Parsec Void String

taskP :: Parser Task
taskP = some ((,) <$> (Hand . map Card <$> some alphaNumChar) <* hspace <*> decimal <* newline) <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

p1 :: Task -> Int
p1 = sum . zipWith (*) [1 ..] . map snd . sort

main :: IO ()
main = readFile "input.txt" >>= print . p1 . parseInput
