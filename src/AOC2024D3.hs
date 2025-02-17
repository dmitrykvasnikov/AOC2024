module AOC2024D3 (solution, part1, part2) where

import           Control.Applicative           ((<|>))
import           Control.Arrow                 ((&&&))
import           Text.Regex.Applicative        (RE, anySym, many, match, string,
                                                sym)
import           Text.Regex.Applicative.Common (decimal)

type Input = [Instruction]

type Parser = RE Char

data Instruction = MUL Int Int
                 | DO
                 | DONT
  deriving (Eq, Show)

insMul :: Parser Instruction
insMul = MUL <$> (string "mul(" *> decimal <* sym ',') <*> (decimal <* sym ')')

insDo :: Parser Instruction
insDo = DO <$ string "do()"

insDont :: Parser Instruction
insDont = DONT <$ string "don't()"

instruction :: Parser (Instruction, String)
instruction = (,) <$> (insMul <|> insDo <|> insDont) <*> many anySym

getInput :: IO Input
getInput = do
  i <- readFile "./input/day3"
  pure $ go i
  where
    go :: String -> Input
    go [] = []
    go s = case match instruction s of
      Nothing          -> go $ tail s
      Just (ins, rest) -> ins : go rest

part1 :: Input -> Int
part1 = foldl' go 0
  where
    go :: Int -> Instruction -> Int
    go r (MUL x y) = r + x * y
    go r _         = r

part2 :: Input -> Int
part2 = part1 . go
  where
    go :: Input -> Input
    go [] = []
    go (i : is) = case i of
      (MUL _ _) -> i : go is
      DO        -> go is
      DONT      -> go (dropWhile (/= DO) is)

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
