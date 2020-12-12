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
      _   -> error $ "Failed to parse action"  <> str 
    value = read $ tail str :: Int

rotate :: Action -> Dir -> Dir
rotate (leftOrRight, i) currentDir = dropWhile (/= currentDir) (cycle dirs) !! div i 90
  where
    directionsRight = [North, East, South, West]
    dirs = case leftOrRight of
      Left -> reverse directionsRight
      Right -> directionsRight
      _     -> error "yay partial functions!" 
  
-- rotate (Left deg) currentDir = (dropWhile (/= currentDir 0) (cycle (reverse dirsRight))) !! (floor (deg / 90))
-- rotate _ = error "rotate is only defined for Left and Right"

part1 :: [Action] -> Int
part1 actions = let (x, y, _) = finalState in abs x + abs y
  where
    finalState = foldl foldActions (0, 0, East) actions
    foldActions :: (Int, Int, Dir) -> Action -> (Int, Int, Dir)
    foldActions (x, y, dir) a = case a of
      (North, i) -> (x, y + i, dir)
      (South, i) -> (x, y - i, dir)
      (East, i)  -> (x + i, y, dir)
      (West, i)  -> (x - i, y, dir)
      (Left, _)  -> (x, y, rotate a dir)
      (Right, _) -> (x, y, rotate a dir)
      (Forward, i) -> foldActions (x, y, dir) (dir, i)
      
part2 :: String -> String
part2 input = "TODO"

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let actions = map parseAction $ lines input 
  putStrLn $ "Part 1:\n" <> show (part1 actions)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 input
