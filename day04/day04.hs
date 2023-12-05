module Main (main) where

import Data.Either (fromRight)
import Data.Tuple.Extra (first)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Points = Int

type Task = [Points]

type Parser = Parsec Void String

cardP :: Parser Points
cardP =
  (\w h -> length $ filter (`elem` h) w)
    <$> ( (string "Card" <* space <* decimal <* char ':' <* space)
            *> some (decimal <* space)
            <* (space *> char '|' <* space)
        )
    <*> some (decimal <* space)

taskP :: Parser Task
taskP = some (cardP <* optional newline) <* eof

parseInput :: String -> Task
parseInput = fromRight [] . runParser taskP "input.txt"

p1 :: Task -> Int
p1 = sum . map ((2 ^) . (+ (-1))) . filter (> 0)

p2 :: Task -> Int
p2 = countOccs . map (1,)
  where
    countOccs ((occs, pts) : ms) = occs + countOccs (mapFirstN (first (+ occs)) pts ms)
    countOccs [] = 0
    mapFirstN f n [] = []
    mapFirstN f 0 xs = xs
    mapFirstN f n (x : xs) = f x : mapFirstN f (n - 1) xs

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
