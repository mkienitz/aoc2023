module Main (main) where

import Control.Monad (guard)
import Data.Either.Extra (fromRight')
import Data.List (foldl')
import Data.List.Extra (chunksOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tuple.Extra ((&&&))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type MapRange = (Int, Int, Int)

type Map = [MapRange]

type Seeds = [Int]

type SeedRange = (Int, Int)

type Task = (Seeds, [Map])

type Parser = Parsec Void String

seedP :: Parser Seeds
seedP = string "seeds: " *> decimal `sepBy` hspace <* newline

mapP :: Parser Map
mapP =
  some printChar
    <* newline
    *> some
      ( (,,)
          <$> (decimal <* hspace)
          <*> (decimal <* hspace)
          <*> (decimal <* space)
      )

taskP :: Parser Task
taskP = (,) <$> seedP <* newline <*> many mapP <* eof

parseInput :: String -> Task
parseInput = fromRight' . runParser taskP "input.txt"

p1 :: Task -> Int
p1 = solve (map (id &&& succ))

p2 :: Task -> Int
p2 = solve (map (\[s, l] -> (s, s + l)) . chunksOf 2)

solve :: (Seeds -> [SeedRange]) -> Task -> Int
solve pre (seeds, maps) = minimum . map fst $ foldl' applyMap (pre seeds) maps
  where
    applyMap :: [SeedRange] -> [MapRange] -> [SeedRange]
    applyMap seeds maps = concatMap (filter (uncurry (<)) . updateSeeds) seeds
      where
        updateSeeds :: SeedRange -> [SeedRange]
        updateSeeds r@(a, b) = head $ mapMaybe overlap maps ++ [[r]]
          where
            overlap :: MapRange -> Maybe [SeedRange]
            overlap (dst, src, n) =
              let (ia, ib) = (max a src, min b (src + n))
               in [(ia + dst - src, ib + dst - src), (a, ia), (ib, b)] <$ guard (ia < ib)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
