module Main (main) where

import Data.Either.Extra (fromRight')
import Data.List (find, foldl')
import Data.List.Extra (chunksOf)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Range = (Int, Int, Int)

type Map = [Range]

type Parser = Parsec Void String

type Seeds = [Int]

type Task = (Seeds, [Map])

seedP :: Parser Seeds
seedP = string "seeds: " *> sepBy1 decimal (char ' ') <* newline

mapP :: Parser Map
mapP =
  some letterChar
    <* string "-to-"
    <* some letterChar
    <* string " map:"
    <* newline
    *> some ((,,) <$> (decimal <* space1) <*> (decimal <* space1) <*> (decimal <* space1))

taskP :: Parser Task
taskP = (,) <$> seedP <* newline <*> many mapP <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

look :: [Range] -> (Int -> Int)
look ms = \x -> maybe x (\(dst, src, _) -> dst + (x - src)) (f x)
  where
    f x = find (\(dst, src, n) -> x >= src && x < src + n) ms

solve :: ([Int] -> [Int]) -> Task -> Int
solve f (seeds, maps) = minimum . map (flip (foldl' (flip look)) maps) $ f seeds

p1 :: Task -> Int
p1 = solve id

p2 :: Task -> Int
p2 = solve (concatMap (\[a, b] -> [a .. (a + b - 1)]) . chunksOf 2)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
