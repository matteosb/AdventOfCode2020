#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package containers,split 

import Data.List.Split (splitOn)
import qualified Data.Set as S

splitAnswers :: String -> [[S.Set Char]]
splitAnswers input = map (map charSet . lines) $ splitOn "\n\n" input
  where
    charSet = foldl (flip S.insert) S.empty

part1 :: [[S.Set Char]] -> Int
part1 groups = sum $ map countDistinctAns groups
  where
    countDistinctAns g = S.size $ foldl (<>) S.empty g

part2 :: [[S.Set Char]] -> Int
part2 groups = sum $ map countSharedAns groups
  where
    countSharedAns g = S.size $ foldl1 S.intersection g

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let groups = splitAnswers input
  putStrLn $ "Part 1:\n" <> show (part1 groups)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 groups)
