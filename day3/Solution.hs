#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

readMap :: IO [String]
readMap = map cycle . lines <$> readFile "./input.txt"

path :: (Int, Int) -> [String] -> [Char]
path _ [] = []
path s@(slopeX, slopeY) m = head (head m) : path s (map (drop slopeX) (drop slopeY m))

countTrees :: String -> Int
countTrees m = length $ filter (== '#') m

part1 :: [String] -> String
part1 m = show . countTrees $ path (3, 1) m

part2 :: [String] -> String
part2 m = show . product $ map (\s -> countTrees $ path s m) slopes
  where
    slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = do
  m <- readMap
  putStrLn $ "Part 1:\n" <> part1 m
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 m
