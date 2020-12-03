#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

readMap :: IO [String]
readMap = map cycle . lines <$> readFile "./input.txt"

path :: [String] -> (Int, Int) -> [Char]
path [] _ = []
path m s@(slopeX, slopeY) = head (head m) : path (map (drop slopeX) (drop slopeY m)) s

countTrees :: String -> Int
countTrees = length . filter (== '#')

part1 :: [String] -> String
part1 m = show . countTrees $ path m (3, 1)

part2 :: [String] -> String
part2 m = show . product $ map (countTrees . path m) slopes
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  m <- readMap
  putStrLn $ "Part 1:\n" <> part1 m
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 m
