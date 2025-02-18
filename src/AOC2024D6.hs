module AOC2024D6 (solution, part1, part2) where

import           Control.Arrow ((&&&))
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S

type Coord = (Int, Int)

type Visited = Set Coord

data Area = Empty | Occupied

type Room = Map Coord Area

data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)

type Input = String

turn :: Direction -> Direction
turn W = N
turn d = succ d

newPosition :: Direction -> Coord -> Coord
newPosition N (x, y) = (x, y - 1)
newPosition E (x, y) = (x + 1, y)
newPosition S (x, y) = (x, y + 1)
newPosition W (x, y) = (x - 1, y)

pathLength :: Room -> Coord -> Int
pathLength r c = go c N S.empty
  where
    go :: Coord -> Direction -> Visited -> Int
    go c d v = case M.lookup c r of
      Nothing       -> S.size v
      Just Empty    -> go (newPosition d c) d (S.insert c v)
      Just Occupied -> go c (turn d) v

getInput :: IO Input
getInput = do
  i <- readFile "./input/day6"
  pure "Not implemented yet"

part1 :: Input -> Int
part1 i = 0

part2 :: Input -> Int
part2 i = 0

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
