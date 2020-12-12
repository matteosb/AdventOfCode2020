#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

import Prelude hiding (Left, Right)

data Dir = North | South | East | West | Left | Right | Forward deriving (Show, Eq)

type Action = (Dir, Int)

parseAction :: String -> Action
parseAction str = (dir, value)
  where
    dir = case head str of
      'N' -> North
      'S' -> South
      'E' -> East
      'W' -> West
      'L' -> Left
      'R' -> Right
      'F' -> Forward
      _ -> error $ "Failed to parse action" <> str
    value = read $ tail str :: Int

rotate :: Action -> Dir -> Dir
rotate (leftOrRight, i) currentDir = dropWhile (/= currentDir) (cycle dirs) !! div i 90
  where
    directionsRight = [North, East, South, West]
    dirs = case leftOrRight of
      Left -> reverse directionsRight
      Right -> directionsRight
      _ -> error "Action must be left or right"

part1 :: [Action] -> Int
part1 actions = let (x, y, _) = finalState in abs x + abs y
  where
    finalState = foldl foldActions (0, 0, East) actions
    foldActions :: (Int, Int, Dir) -> Action -> (Int, Int, Dir)
    foldActions (x, y, dir) a = case a of
      (North, i) -> (x, y + i, dir)
      (South, i) -> (x, y - i, dir)
      (East, i) -> (x + i, y, dir)
      (West, i) -> (x - i, y, dir)
      (Left, _) -> (x, y, rotate a dir)
      (Right, _) -> (x, y, rotate a dir)
      (Forward, i) -> foldActions (x, y, dir) (dir, i)

rotateWP :: Int -> Bool -> (Int, Int) -> (Int, Int)
rotateWP 0 _ wp = wp
rotateWP degrees left (x, y) = rotateWP (degrees - 90) left $ if left then (- y, x) else (y, - x)

part2 :: [Action] -> Int
part2 actions = let (x, y, _) = finalState in abs x + abs y
  where
    finalState = foldl foldActions (0, 0, (10, 1)) actions
    foldActions :: (Int, Int, (Int, Int)) -> Action -> (Int, Int, (Int, Int))
    foldActions (x, y, (wx, wy)) a = case a of
      (North, i) -> (x, y, (wx, wy + i))
      (South, i) -> (x, y, (wx, wy - i))
      (East, i) -> (x, y, (wx + i, wy))
      (West, i) -> (x, y, (wx - i, wy))
      (Left, deg) -> (x, y, rotateWP deg True (wx, wy))
      (Right, deg) -> (x, y, rotateWP deg False (wx, wy))
      (Forward, i) -> (x + i * wx, y + i * wy, (wx, wy))

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let actions = map parseAction $ lines input
  putStrLn $ "Part 1:\n" <> show (part1 actions)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 actions)
