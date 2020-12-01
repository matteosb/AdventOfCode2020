#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

part1 :: String -> String
part1 input = "TODO"

part2 :: String -> String
part2 input = "TODO"

main :: IO ()
main = do
  input <- readFile "./input.txt"
  putStrLn $ "Part 1:\n" <> part1 input
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 input
