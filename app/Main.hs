module Main where

import qualified AOC2024D1 as D1
import qualified AOC2024D2 as D2
import qualified AOC2024D3 as D3
import qualified AOC2024D4 as D4
import qualified AOC2024D5 as D5
import qualified AOC2024D6 as D6
import qualified AOC2024D7 as D7
import           System.IO

getHandler :: String -> Maybe (IO (Int, Int))
getHandler "1" = Just $ D1.solution
getHandler "2" = Just $ D2.solution
getHandler "3" = Just $ D3.solution
getHandler "4" = Just $ D4.solution
getHandler "5" = Just $ D5.solution
getHandler "6" = Just $ D6.solution
getHandler "7" = Just $ D7.solution
getHandler _   = Nothing

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr "\nEnter day number (ENTER to leave): "
  day <- getLine
  case day of
    "" -> putStrLn "Bye!"
    day' -> case getHandler day' of
      Nothing  -> (putStrLn $ "No solution for day: " <> day') >> main
      Just sol -> sol >>= putStrLn . show
