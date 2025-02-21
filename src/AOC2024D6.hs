module AOC2024D6 (solution, part1, part2) where

import           Control.Arrow ((&&&))
import qualified Control.Monad
import qualified Data.Array    as A
import qualified Data.Set      as S
import           Debug.Trace
import           Linear.V2     (V2 (..), _y, perp)
import           Linear.Vector (unit)

data Entity = Empty | Obstacle deriving (Eq, Show)

type Room = A.Array Coord Entity

type Input = (Guard, Room)

type Coord = V2 Int

type Direction = V2 Int

data Guard = Guard { dir :: Direction
                   , pos :: Coord
                   }
  deriving (Eq, Ord, Show)

charToEntity :: Char -> Entity
charToEntity '#' = Obstacle
charToEntity _   = Empty

north :: Direction
north = negate . unit $ _y

turn :: Direction -> Direction
turn = perp

move :: Direction -> Coord -> Coord
move d c = d + c

inGrid :: Room -> Coord -> Bool
inGrid = A.inRange . A.bounds

step :: Room -> Guard -> Guard
step r (Guard d c) = Guard d' (move d' c)
  where
    d' = head . filter isEmpty . iterate turn $ d
    isEmpty d'' = not (inGrid r p' && r A.! p' == Obstacle)
      where
        p' = move d'' c

getInput :: IO Input
getInput = do
  i <- lines <$> readFile "./input/day6"
  let indexed = concat (zipWith (\c l -> zipWith (\r s -> (V2 r c, s)) [1 ..] l) [1 ..] i)
  let guard = Guard north (head [p | (p, '^') <- indexed])
  pure (guard, A.array (V2 1 1, V2 (length . head $ i) (length i)) (map (fmap charToEntity) indexed))

part1 :: Input -> Int
part1 (g, r) = S.size . S.fromList . takeWhile (inGrid r) . map pos . iterate (step r) $ g

part2 :: Input -> Int
part2 (g, r) = length . filter id $ do
  o <- obstacles
  Control.Monad.guard (o /= pos g)
  let r' = r A.// [(o, Obstacle)]
  pure . isLoop $ r'
  where
    obstacles = S.toList . S.fromList . filter (/= pos g) . takeWhile (inGrid r) . map pos . iterate (step r) $ g
    isLoop r' = go S.empty (takeWhile (inGrid r' . pos) (iterate (step r') g))
    go _ [] = False
    go seen (g : gs)
      | S.member g seen = True
      | otherwise = go (S.insert g seen) gs

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
