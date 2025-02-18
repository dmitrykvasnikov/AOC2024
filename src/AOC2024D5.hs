module AOC2024D5 (solution, part1, part2) where

import           Control.Arrow                 ((&&&))
import           Data.List                     (sortBy)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe)
import           Text.Regex.Applicative        (RE, comap, few, many, match,
                                                sym)
import           Text.Regex.Applicative.Common (decimal)

type Rule = (Int, Int)

type Update = [Int]

type RuleDictionary = Map Int [Int]

type Parser = RE Char

type Input = ([Rule], [Update])

rule :: Parser Rule
rule = (,) <$> (decimal <* sym '|') <*> decimal <* sym '\n'

update :: Parser Update
update = (:) <$> decimal <*> few (sym ',' *> decimal)

getInput :: IO Input
getInput = do
  i <- readFile "./input/day5"
  case match ((,) <$> many rule <* (many $ sym '\n') <*> many (update <* sym '\n')) i of
    Just i  -> pure i
    Nothing -> pure ([], [])

ruleDict :: [Rule] -> RuleDictionary
ruleDict = foldl' (\m (p, b) -> M.insertWith (<>) p [b] m) M.empty

isUpdateValid :: RuleDictionary -> Update -> Bool
isUpdateValid _ [] = True
isUpdateValid _ [_] = True
isUpdateValid dict (p : ps) =
  if all id (map (flip elem pages) ps)
    then isUpdateValid dict ps
    else False
  where
    pages = fromMaybe [] (M.lookup p dict)

comparePages :: RuleDictionary -> Int -> Int -> Ordering
comparePages dict x y
  | x == y = EQ
  | elem y (fromMaybe [] (M.lookup x dict)) = LT
  | otherwise = GT

middlePage :: [Int] -> Int
middlePage ps = ps !! (length ps `div` 2)

part1 :: Input -> Int
part1 (r, u) = sum . map middlePage . filter (isUpdateValid (ruleDict r)) $ u

part2 :: Input -> Int
part2 (r, u) = sum . map (middlePage . sortBy (comparePages dict)) . filter (not . isUpdateValid dict) $ u
  where
    dict = ruleDict r

solution :: IO (Int, Int)
solution = getInput >>= pure . (part1 &&& part2)
