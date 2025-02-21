module AOC2024D6 (solution, part1, part2) where

import           Control.Arrow ((&&&))
import qualified Data.Array    as A
import qualified Data.Set      as S
import           Debug.Trace

data Entity = Empty | Obstacle deriving (Eq, Show)

type Room = A.Array Coord Entity

type Input = (Guard, Room)

type Coord = (Int, Int)

data Direction = N | E | S | W deriving (Enum, Eq, Ord, Show)

data Guard = Guard { dir :: Direction
                   , pos :: Coord
                   }
  deriving (Eq, Ord, Show)

charToEntity :: Char -> Entity
charToEntity '#' = Obstacle
charToEntity _   = Empty

turn :: Direction -> Direction
turn W = N
turn d = succ d

move :: Direction -> Coord -> Coord
move N (x, y) = (x, y - 1)
move E (x, y) = (x + 1, y)
move S (x, y) = (x, y + 1)
move W (x, y) = (x - 1, y)

inGrid :: Room -> Coord -> Bool
inGrid = A.inRange . A.bounds

step :: Room -> Guard -> Guard
step r (Guard d c) = Guard d' (move d' c)
  where
    d' = head . filter isEmpty . iterate turn $ d
    isEmpty d'' = not (inGrid r p') || r A.! p' == Empty
      where
        p' = move d'' c

getInput :: IO Input
getInput = do
  i <- lines <$> readFile "./input/day6"
  let indexed = concat (zipWith (\c l -> zipWith (\r s -> ((r, c), s)) [1 ..] l) [1 ..] i)
  let guard = Guard N (head [p | (p, '^') <- indexed])
  pure (guard, A.array ((1, 1), (length . head $ i, length i)) (map (fmap charToEntity) indexed))

part1 :: Input -> Int
part1 (g, r) = S.size . S.fromList . takeWhile (inGrid r) . map pos . iterate (step r) $ g

part2 :: Input -> Int
part2 (g, r) =
  let obstacles = S.toList . S.fromList . filter (/= pos g) . takeWhile (inGrid r) . map pos . iterate (step r) $ g
   in length . filter id $ do
        o <- obstacles
        let r' = r A.// [(o, Obstacle)]
        pure . isLoop $ r'
  where
    isLoop r' = go S.empty (takeWhile (inGrid r' . pos) . iterate (step r') $ g)
    go _ [] = False
    go seen (g : gs)
      | elem g seen = True
      | otherwise = go (S.insert g seen) gs

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
