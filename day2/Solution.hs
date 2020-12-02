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

isValid :: Password -> Bool
isValid (Password (Policy mi ma c) str) = mi <= charCount && charCount <= ma
  where
    charCount = count (== c) str

part1 :: [String] -> Int
part1 = count isValid . map parseLine

part2 :: [String] -> String
part2 input = "TODO"

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let ls = lines input
  putStrLn $ "Part 1:\n" <> show (part1 ls)

-- putStrLn "---------------------------------------------"
-- putStrLn $ "Part 2:\n" <> part2 input
