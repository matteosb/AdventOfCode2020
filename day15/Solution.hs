#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

import Data.Maybe

nextNumber :: [Int] -> Int
nextNumber spoken = fromMaybe 0 turnDiff
  where 
    turnDiff = previousAppearance (head spoken) 1 (tail spoken)
    previousAppearance :: Int -> Int -> [Int] -> Maybe Int
    previousAppearance _ _ [] = Nothing    
    previousAppearance n c (x:xs) =
      if n == x then Just c else previousAppearance n (c + 1) xs 


genSeq :: [Int] -> Int -> [Int]
genSeq starting total = go (reverse starting) total (length starting)
  where go xs t l
          | t == l = xs
          | otherwise = go (nextNumber xs : xs) t (l + 1) 

part1 :: [Int] -> Int
part1 startingNumbers = head (genSeq startingNumbers 2020)

part2 :: [Int] -> Int
part2 startingNumbers = head (genSeq startingNumbers 30000000)

main :: IO ()
main = do
  let startingNumbers = [19, 20, 14, 0, 9, 1]
  putStrLn $ "Part 1:\n" <> show (part1 startingNumbers)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 startingNumbers)
