module Main (main) where

import Data.Graph qualified as G
import Data.List (find, foldl1')
import Data.Map qualified as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as S
import Data.Tuple.Extra (both, first, second)

type Coords = (Int, Int)

type Entry = (Coords, (Coords, Coords))

type Graph = M.Map Coords (Coords, Coords)

withNeighbors :: (Coords, Char) -> (Coords, Coords, Coords, Coords) -> Entry
withNeighbors (coords, p) ns = (coords, pickNeighbors p ns)
  where
    pickNeighbors p (n, e, s, w) =
      case p of
        '|' -> (n, s)
        '-' -> (w, e)
        'L' -> (n, e)
        'J' -> (n, w)
        '7' -> (s, w)
        'F' -> (s, e)

pipeFromContext :: (Char, Char, Char, Char) -> Char
pipeFromContext (n, e, s, w)
  | connS n && connN s = '|'
  | connE w && connW e = '-'
  | connS n && connW e = 'L'
  | connS n && connE w = 'J'
  | connN s && connE w = '7'
  | connN s && connW e = 'F'
  where
    connN c
      | c `elem` ['|', 'L', 'J'] = True
      | otherwise = False
    connE c
      | c `elem` ['-', 'L', 'F'] = True
      | otherwise = False
    connS c
      | c `elem` ['|', 'F', '7'] = True
      | otherwise = False
    connW c
      | c `elem` ['-', 'J', '7'] = True
      | otherwise = False

withBorder s = (++ [topBot]) . (topBot :) . map (("." <>) . (<> ".")) $ lines s
  where
    topBot = replicate (n + 2) '.'
    n = length . head $ lines s

parseInput :: String -> [(Coords, Char)]
parseInput =
  concat
    . zipWith (\ridx line -> zipWith (\cidx c -> ((ridx, cidx), c)) [0 ..] line) [0 ..]
    . withBorder

toGraph :: [(Coords, Char)] -> (Graph, Coords)
toGraph input = (M.fromList $ mapMaybe toEntry input, startPos)
  where
    startPos = fst . fromJust $ find ((== 'S') . snd) input
    lut = M.fromList input
    toEntry ((r, c), p) = case p of
      'S' -> Just $ withNeighbors ((r, c), pipeFromContext nsPipes) ns
      '.' -> Nothing
      p -> Just $ withNeighbors ((r, c), p) ns
      where
        ns@(n, e, s, w) = ((r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1))
        nsPipes = (lut M.! n, lut M.! e, lut M.! s, lut M.! w)

solve :: (Graph, Coords) -> [Coords]
solve (graph, start) =
  (start :)
    . takeWhile (/= start)
    . map snd
    $ iterate go (start, fst $ graph M.! start)
  where
    go (prev, curr) = (curr, next prev curr)
    next prev curr = let (a, b) = graph M.! curr in if a /= prev then a else b

p1 :: [(Coords, Char)] -> Int
p1 = (`div` 2) . length . solve . toGraph

-- p2 task = filter isEnclosed . map fst $ filter ((== '.') . snd) task
--   where
--     loop = solve $ toGraph task
--     isEnclosed :: Coords -> Bool
--     isEnclosed (r, c) = foldl1' (&&) [n, e, s, w]
--       where
--         n = any (\(rL, cL) -> rL > r && cL == c) loop
--         e = any (\(rL, cL) -> rL == r && cL > c) loop
--         s = any (\(rL, cL) -> rL < r && cL == c) loop
--         w = any (\(rL, cL) -> rL == r && cL < c) loop

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1] . parseInput
