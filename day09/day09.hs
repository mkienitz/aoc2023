module Main (main) where

type History = [Int]

type Task = [History]

parseInput :: String -> Task
parseInput = map (map read . words) . lines

solve :: (History -> Int) -> (Int -> Int -> Int) -> [History] -> Int
solve ex op = sum . map go
  where
    go hist
      | all (== 0) hist = 0
      | otherwise = ex hist `op` go (diffs hist)
      where
        diffs hist = zipWith (flip (-)) hist (tail hist)

p1 :: Task -> Int
p1 = solve last (+)

p2 :: Task -> Int
p2 = solve head (-)

main :: IO ()
main = readFile "input.txt" >>= print . sequence [p1, p2] . parseInput
