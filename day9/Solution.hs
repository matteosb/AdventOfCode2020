#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

import Data.List

part1 :: Int -> [Int] -> Maybe Int
part1 preambleLen nums = fst <$> find invalid zipped
  where
    (preamble, rest) = splitAt preambleLen nums
    zipped = zip rest $ scanl (\acc num -> take preambleLen (num : acc)) (reverse preamble) rest
    invalid (n, prev) = null $ intersect prev (map (n -) prev)

part2 :: String -> String
part2 input = "TODO"

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let nums = map read (lines input) :: [Int]
  putStrLn $ "Part 1:\n" <> show (part1 25 nums)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 input
