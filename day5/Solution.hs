#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

{-# LANGUAGE MultiWayIf #-}

import qualified Data.List as L

bsearch :: Int -> Int -> Char -> Char -> String -> Int
bsearch rmin rmax _ _ [] = if rmin == rmax then rmin else error "something went wrong"
bsearch rmin rmax lchar rchar (x : xs) =
  if
      | x == lchar -> bsearch rmin midpoint lchar rchar xs
      | x == rchar -> bsearch (midpoint + 1) rmax lchar rchar xs
      | otherwise -> error "malformed row spec"
  where
    midpoint = rmin + ((rmax - rmin) `div` 2)

findRow :: String -> Int
findRow = bsearch 0 127 'F' 'B'

findCol :: String -> Int
findCol = bsearch 0 7 'L' 'R'

findSeat :: String -> (Int, Int)
findSeat spec = (findRow (take 7 spec), findCol (drop 7 spec))

seatId :: (Int, Int) -> Int
seatId (r, c) = (8 * r) + c

part1 :: String -> Int
part1 input = L.maximum $ map (seatId . findSeat) (lines input)

part2 :: String -> Int
part2 input =
  case firstSkipped of
    Just (prev, _) -> prev + 1
    Nothing -> error "No empty seat found"
  where
    seatIds = L.sort $ map (seatId . findSeat) (lines input)
    zippedWithNext = zip seatIds (drop 1 seatIds)
    firstSkipped = L.find (\(x, y) -> y - x /= 1) zippedWithNext

main :: IO ()
main = do
  input <- readFile "./input.txt"
  putStrLn $ "Part 1:\n" <> show (part1 input)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 input)
