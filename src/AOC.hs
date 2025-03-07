module AOC
  ( Coord,
    VCoord,
    mkIndex',
    mkV2Index,
    mkCoordIndex,
  )
where

import           Linear (V2 (..))

type Coord = (Int, Int)

type VCoord = V2 Int

-- mkIndex takes predicate and conversion fuction for both element anc coord (const True && id in base case)
-- and starting index
mkIndex' :: (a -> Bool) -> (a -> b) -> (Coord -> c) -> Int -> [[a]] -> [(c, b)]
mkIndex' p fnOnEl fnOnCoord stInd ls = [(fnOnCoord (x, y), fnOnEl a) | (y, l) <- zipWith (,) [stInd ..] ls, (x, a) <- zipWith (,) [stInd ..] l, p a]

mkCoordIndex :: (a -> Bool) -> (a -> b) -> Int -> [[a]] -> [(Coord, b)]
mkCoordIndex p fnOnEl stInd = mkIndex' p fnOnEl id stInd

mkV2Index :: (a -> Bool) -> (a -> b) -> Int -> [[a]] -> [(VCoord, b)]
mkV2Index p fnOnEl stInd = mkIndex' p fnOnEl coordToV2 stInd

coordToV2 :: Coord -> VCoord
coordToV2 (x, y) = V2 x y
