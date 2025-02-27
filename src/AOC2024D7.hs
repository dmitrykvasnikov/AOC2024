module AOC2024D7 (solution, part1, part2) where

import           Control.Arrow                 ((&&&))
import           Text.Regex.Applicative        (RE (..), match, some, sym)
import           Text.Regex.Applicative.Common (decimal)

data Equation a = Equation a [a]
  deriving (Show)

data Operator = Add | Mult | Concat deriving (Show)

-- Solution has a resulting value, first element and list of operators with second operand
data Solution a = Solution a a [(a, Operator)]
  deriving (Show)

type Input = [Equation Int]

type Parser = RE Char

equation :: Parser (Equation Int)
equation = Equation <$> (decimal <* sym ':') <*> (some (sym ' ' *> decimal))

getInput :: IO Input
getInput = maybe (error "parse error") id . match (some (equation <* sym '\n')) <$> readFile "./input/day7"

apply :: (Num a, Show a, Read a) => Operator -> a -> a -> a
apply Add    = (+)
apply Mult   = (*)
apply Concat = \x y -> read (show x <> show y)

solutions :: [Operator] -> Equation Int -> [Solution Int]
solutions ops eq@(Equation goal (num : nums)) = Solution goal num <$> go num nums
  where
    go total nums' = case (compare total goal, nums') of
      (GT, _) -> []
      (LT, []) -> []
      (EQ, []) -> [[]]
      (_, (x : xs)) -> do
        op <- ops
        ((x, op) :) <$> go (apply op total x) xs

solve :: [Operator] -> Input -> Int
solve ops = sum . map score
  where
    score :: (Equation Int) -> Int
    score eq@(Equation goal _)
      | null (solutions ops eq) = 0
      | otherwise = goal

part1 :: Input -> Int
part1 = solve [Mult, Add]

part2 :: Input -> Int
part2 = solve [Mult, Concat, Add]

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
