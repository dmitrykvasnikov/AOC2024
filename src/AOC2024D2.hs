module AOC2024D2 (solution, part1, part2) where

import           Control.Arrow   ((&&&))
import           Data.List.Split

type Level = Int

type Report = [Level]

type Input = [Report]

getInput :: IO Input
getInput = readFile "./input/day2" >>= pure . map (map read . splitOn " ") . lines

isValidReport :: Report -> Bool
isValidReport r@(x : y : _) =
  go (x - y) r
  where
    go :: Int -> Report -> Bool
    go off (x : y : rest) =
      if (off * (x - y) > 0) && (abs (x - y) <= 3) && (abs (x - y) >= 1) then go off (y : rest) else False
    go _ _ = True
isValidReport _ = True

problemDampener :: Report -> [Report]
problemDampener []       = []
problemDampener (x : xs) = xs : map (x :) (problemDampener xs)

part1 :: Input -> Int
part1 = length . filter isValidReport

part2 :: Input -> Int
part2 = length . filter (any isValidReport . problemDampener)

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
