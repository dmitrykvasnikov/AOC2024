module AOC2024D7 (solution, part1, part2) where

import           Control.Arrow                 ((&&&))
import           Text.Regex.Applicative        (RE (..), many, match, string,
                                                sym)
import           Text.Regex.Applicative.Common (decimal)

type Calibration = Int

type Operand = Int

type Operation = Operand -> Operand -> Operand

type Input = [(Calibration, [Operand])]

type Parser = RE Char

calibration :: Parser Calibration
calibration = decimal

operands :: Parser [Operand]
operands = (:) <$> decimal <*> many (sym ' ' *> decimal)

operations_part1 :: [Operation]
operations_part1 = [(+), (*)]

operations_part2 :: [Operation]
operations_part2 = [(+), (*), (\x y -> read $ show x <> show y)]

input :: Parser Input
input = many ((,) <$> (calibration <* string ": ") <*> (operands <* sym '\n'))

getInput :: IO Input
getInput = (maybe [] id . match input) <$> readFile "./input/day7"

decode :: [Operation] -> Calibration -> [Operand] -> Bool
decode operations cal ops = any id (go (head ops) (tail ops))
  where
    go res [] = [res == cal]
    go res (o : ops)
      | res >= cal = [False]
      | otherwise = concatMap (flip go ops) (map (\op -> op res o) operations)

part1 :: Input -> Int
part1 = sum . map fst . filter (uncurry (decode operations_part1))

part2 :: Input -> Int
part2 = sum . map fst . filter (uncurry (decode operations_part2))

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
