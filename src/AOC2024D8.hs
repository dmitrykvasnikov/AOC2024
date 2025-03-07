module AOC2024D8 (solution, part1, part2) where

import           AOC             (VCoord, mkV2Index)
import           Control.Arrow   ((&&&))
import qualified Data.Map        as M
import           Data.Map.Strict (Map)
import qualified Data.Set        as S
import           Debug.Trace
import qualified Linear          as V

type Antenna = Char

data Input = Grid { width  :: Int
                  , height :: Int
                  , locs   :: Map Antenna [VCoord]
                  }
  deriving (Show)

getInput :: IO Input
getInput = do
  i <- lines <$> readFile "./input/day8"
  pure . Grid (length . head $ i) (length i) $
    foldl' (\m (coord, k) -> M.insertWith (<>) k [coord] m) M.empty (mkV2Index (/= '.') id 1 i)

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs [x]      = []
pairs (x : xs) = map (x,) xs <> pairs xs

inGrid :: Int -> Int -> VCoord -> Bool
inGrid w h (V.V2 x y) = (x >= 1) && (x <= w) && (y >= 1) && (y <= h)

iterNodes, symNodes :: (VCoord, VCoord) -> [VCoord]
symNodes (g, h) = [g - j, h + j]
  where
    j = h - g
iterNodes (g, h) = let delta = h - g in iterate (+ delta) h

iterateNodes :: (VCoord, VCoord) -> [VCoord]
iterateNodes (g, h) = let j = h - g in iterate (+ j) h

part1 :: Input -> Int
part1 (Grid w h ls) = S.size . S.fromList . concatMap (\(g, h) -> go (g, h) <> go (h, g)) . concat . map pairs . M.elems $ ls
  where
    go :: (VCoord, VCoord) -> [VCoord]
    go = filter (inGrid w h) . take 1 . tail . iterNodes

part2 :: Input -> Int
part2 (Grid w h ls) = S.size . S.fromList . concatMap (\(g, h) -> go (g, h) <> go (h, g)) . concat . map pairs . M.elems $ ls
  where
    go :: (VCoord, VCoord) -> [VCoord]
    go = takeWhile (inGrid w h) . iterNodes

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
