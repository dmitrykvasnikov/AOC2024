module AOC
  ( Coord,
    mkIndex,
  )
where

type Coord = (Int, Int)

mkIndex :: Int -> [[a]] -> (a -> b) -> [(Coord, b)]
mkIndex start ls f = [((x, y), f a) | (y, l) <- zipWith (,) [start ..] ls, (x, a) <- zipWith (,) [start ..] l]
