#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package regex-tdfa

import Text.Regex.TDFA

data Policy = Policy Int Int Char deriving (Show)

data Password = Password Policy String deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

parseLine :: String -> Password
parseLine l =
  case parsed of
    ("", _, _, mi : ma : c : pass : _) -> Password (Policy (read mi) (read ma) (head c)) pass
    _ -> error $ "Failed to parse line: " <> l
  where
    parserRegex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)"
    parsed = l =~ parserRegex :: (String, String, String, [String])

part1Validator :: Password -> Bool
part1Validator (Password (Policy mi ma c) str) = mi <= charCount && charCount <= ma
  where
    charCount = count (== c) str

part2Validator :: Password -> Bool
part2Validator (Password (Policy i1 i2 c) str) = count (== c) [str !! (i1 - 1), str !! (i2 - 1)] == 1

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let passwords = map parseLine $ lines input
  putStrLn $ "Part 1:\n" <> show (count part1Validator passwords)
  putStrLn $ "Part 2:\n" <> show (count part2Validator passwords)
