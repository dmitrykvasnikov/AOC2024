module AOC2024D6 (solution, part1, part2) where

import           Control.Arrow ((&&&))
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Debug.Trace

type Coord = (Int, Int)

type Visited = Set (Coord, Direction)

data Area = Empty | Occupied | Start deriving (Eq, Show)

type Room = Map Coord Area

data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)

-- Input - (Starting ccordinate, room)
type Input = (Coord, Room)

data Length = Loop
            | Length Int

turn :: Direction -> Direction
turn W = N
turn d = succ d

newPosition :: Direction -> Coord -> Coord
newPosition N (x, y) = (x, y - 1)
newPosition E (x, y) = (x + 1, y)
newPosition S (x, y) = (x, y + 1)
newPosition W (x, y) = (x - 1, y)

-- pathLength :: Coord -> Room -> [(Coord, Direction)]
-- pathLength c r = go c N S.empty
--   where
--     go :: Coord -> Direction -> Visited -> [(Coord, Direction)]
--     go cur d v =
--       let newCur = newPosition d cur
--        in case M.lookup newCur r of
--             Nothing       -> [(cur, d)]
--             Just Empty    -> (cur, d) : go newCur d (S.insert cur v)
--             Just Occupied -> go cur (turn d) v

pathLength :: Coord -> Room -> Length
pathLength c r = go c N S.empty
  where
    go :: Coord -> Direction -> Visited -> Length
    go cur d v =
      let newCur = newPosition d cur
       in case M.lookup newCur r of
            -- add 1 to account current position
            Nothing -> Length $ 1 + (S.size . S.map fst $ v)
            Just Occupied -> go cur (turn d) v
            _ -> if (S.member (newCur, d) v) then Loop else go newCur d (S.insert (cur, d) v)

isLoop :: Coord -> Room -> Bool
isLoop c r = case pathLength c r of
  Loop -> True
  _    -> False

getEmptyCoords :: Room -> [Coord]
getEmptyCoords = M.foldlWithKey (\coords coord area -> if area == Empty then coord : coords else coords) []

charToArea :: Char -> Area
charToArea = \case
  '#' -> Occupied
  '^' -> Start
  _   -> Empty

getInput :: IO Input
getInput = do
  i <- (concatMap (\(r, l) -> zipWith (\c s -> ((c, r), s)) [1 ..] l) . zipWith (,) [1 ..] . lines) <$> readFile "./input/day6"
  pure $ foldl' (\(start, area) (c, s) -> (if s == '^' then c else start, M.insert c (charToArea s) area)) ((0, 0), M.empty) i

-- doesn't need to check for Loop in first part
part1 :: Input -> Int
part1 = (\(Length i) -> i) . uncurry pathLength

part2 :: Input -> Int
part2 (c, r) = length . filter id . map (\ec -> isLoop c $ M.adjust (const Occupied) ec r

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
