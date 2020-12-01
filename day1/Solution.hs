#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

readInput :: IO [Int]
readInput = do
  content <- readFile "./input.txt"
  return $ map read $ words content

part1 :: [Int] -> String
part1 input = show $ head [x * y | x <- input, y <- input, x + y == 2020]

part2 :: [Int] -> String
part2 input = show $ head [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]

main :: IO ()
main = do
  input <- readInput
  putStrLn $ "Part 1:\n" <> part1 input
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 input
