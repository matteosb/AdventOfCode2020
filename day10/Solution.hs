#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

{-# LANGUAGE MultiWayIf #-}

import Data.List

diffs :: [Int] -> [Int]
diffs adapters = zipWith (-) chain (0 : chain)
  where
    chain = sort adapters

part1 :: [Int] -> Int
part1 adapters = count (== 1) * (count (== 3) + 1)
  where
    count p = length $ filter p (diffs adapters)

part2 :: [Int] -> Int
part2 adapters = product . map validCombinations . chunk $ sort adapters

validCombinations :: [Int] -> Int
validCombinations sortedAdapters = go sortedAdapters initValue
  where
    -- bit of a hack, chunk could do some extra processing on the lists instead
    initValue = if head sortedAdapters < 3 then 0 else head sortedAdapters - 3
    go [] _ = 0
    go (x : xs) prev =
      if
          | null xs -> if d > 3 then 0 else 1
          | d > 3 -> 0
          | d == 3 -> go xs x
          | otherwise -> go xs x + go xs prev
      where
        d = x - prev

chunk :: [Int] -> [[Int]]
chunk sortedAdapters = map reverse $ foldl' fn [[head sortedAdapters]] (tail sortedAdapters)
  where
    fn [] _ = error "this should never happen"
    fn (h : l) x = if x - head h == 3 then [x] : h : l else (x : h) : l

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let adapters = map read (lines input) :: [Int]
  putStrLn $ "Part 1:\n" <> show (part1 adapters)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 adapters)
