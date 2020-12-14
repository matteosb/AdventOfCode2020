#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package split

import Data.List
import Data.List.Split

part1 :: Int -> String -> Int
part1 earliestTime schedule = busId * waitTime
  where
    buses = map read $ filter (/= "x") $ splitOn "," schedule
    (busId, waitTime) = head . sortOn snd $ map (\b -> (b, b - earliestTime `mod` b)) buses

part2 :: String -> Int
part2 schedule = go (1, 0) $ sortOn fst scheduleWithOffset
  where
    scheduleWithOffset = map (\(b, i) -> (read b :: Int, i)) . filter ((/= "x") . fst) $ zip (splitOn "," schedule) [0 ..]
    go (c, o) [] = c - o
    go (c1, o1) ((c2, o2) : rest) =
      let next = head . filter (\t -> t `mod` c2 == (c2 - o2) `mod` c2) $ map (\x -> (x * c1) - o1) [1 ..]
       in go (c1 * c2, c1 * c2 - next) rest

main :: IO ()
main = do
  ls <- lines <$> readFile "./input.txt"
  let earliestTime = (read $ head ls) :: Int
      schedule = ls !! 1
  putStrLn $ "Part 1:\n" <> show (part1 earliestTime schedule)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 schedule)
