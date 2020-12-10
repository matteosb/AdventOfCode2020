#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

{-# LANGUAGE MultiWayIf #-}

import Data.List
import Data.Maybe

part1 :: Int -> [Int] -> Maybe Int
part1 preambleLen nums = fst <$> find invalid zipped
  where
    (preamble, rest) = splitAt preambleLen nums
    zipped = zip rest $ scanl (\acc num -> take preambleLen (num : acc)) (reverse preamble) rest
    invalid (n, prev) = null $ intersect prev (map (n -) prev)

findContiguousSum :: [Int] -> Int -> Maybe (Int, Int)
findContiguousSum [] _ = Nothing
findContiguousSum nums needle =
  case findContiguousSum' nums needle of
    Nothing -> findContiguousSum (tail nums) needle
    Just range -> Just (minimum range, maximum range)

findContiguousSum' :: [Int] -> Int -> Maybe [Int]
findContiguousSum' nums needle = go nums 0 []
  where
    go [] _ _ = Nothing
    go (x : xs) sum_ rng =
      let newSum = (sum_ + x)
       in if
              | newSum == needle -> Just (x : rng)
              | newSum > needle -> Nothing
              | otherwise -> go xs newSum (x : rng)

part2 :: Int -> [Int] -> Int
part2 invalidNum input =
  let Just (smallest, largest) = findContiguousSum input invalidNum
   in smallest + largest

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let nums = map read (lines input) :: [Int]
  let invalidNumber = fromMaybe (error "no invalid number") $ part1 25 nums
  putStrLn $ "Part 1:\n" <> show invalidNumber
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 invalidNumber nums)
