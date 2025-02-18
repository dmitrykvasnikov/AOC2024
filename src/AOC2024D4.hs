module AOC2024D4 (solution, part1, part2) where

import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import           Data.List     (isPrefixOf, sort, tails, transpose)

type Input = [String]

rotations :: [[a]] -> [[[a]]]
rotations = take 4 . iterate (reverse . transpose)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals arr@(_ : rest) = transpose (zipWith drop [0 ..] $ arr) <> diagonals rest

possibleLines :: [[a]] -> [[a]]
possibleLines ls = do
  l <- rotations ls
  (l >>= tails) <> diagonals l

subGrids :: Int -> [[a]] -> [[[a]]]
subGrids s g = do
  grid <- tails g
  let slice = take s grid
  guard $ length slice == s
  transpose $ map (map (take s) . tails) slice

isXMAS :: [String] -> Bool
isXMAS [[a, _, b], [_, 'A', _], [c, _, d]] = sort [a, d] == "MS" && sort [b, c] == "MS"
isXMAS _ = False

getInput :: IO Input
getInput = lines <$> readFile "./input/day4"

part1 :: Input -> Int
part1 = length . filter (isPrefixOf "XMAS") . possibleLines

part2 :: Input -> Int
part2 = length . filter isXMAS . subGrids 3

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
