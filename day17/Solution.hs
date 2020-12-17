#!/usr/bin/env stack
-- stack --resolver lts-16.23 script

import Control.Monad
import Data.List (unzip4)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int, Int)

type Point4D = (Int, Int, Int, Int)

readInput :: String -> Set Point
readInput = mkSet . zip [0 ..] . map (zip [0 ..]) . lines
  where
    mkSet :: [(Int, [(Int, Char)])] -> Set Point
    mkSet l = S.fromList $ l >>= mapRow
    mapRow (y, row) = map (\(x, _) -> (x, y, 0)) $ filter (\(_, c) -> c == '#') row

neighbors :: Point -> [Point]
neighbors (x, y, z) = do
  xo <- [-1, 0, 1]
  yo <- [-1, 0, 1]
  zo <- [-1, 0, 1]
  guard $ (xo, yo, zo) /= (0, 0, 0)
  return (x + xo, y + yo, z + zo)

liveNeighbors :: Point -> Set Point -> [Point]
liveNeighbors p s = do
  n <- neighbors p
  guard $ S.member n s
  return n

allPoints :: Set Point -> [Point]
allPoints s = do
  x <- [minX .. maxX]
  y <- [minY .. maxY]
  z <- [minZ .. maxZ]
  return (x, y, z)
  where
    (xs, ys, zs) = unzip3 $ S.toList s
    minX = minimum xs - 1
    maxX = maximum xs + 1
    minY = minimum ys - 1
    maxY = maximum ys + 1
    minZ = minimum zs - 1
    maxZ = maximum zs + 1

nextState :: Set Point -> Set Point
nextState s = S.fromList . filter willBeActive $ allPoints s
  where
    willBeActive p =
      let liveN = length $ liveNeighbors p s
       in if S.member p s then liveN == 3 || liveN == 2 else liveN == 3

part1 :: Set Point -> Int
part1 initialState = S.size $ loop 0 initialState
  where
    loop n state
      | n == 6 = state
      | otherwise = loop (n + 1) $ nextState state

-- TODO: make these functions generic over n-dimensional points
neighbors2 :: Point4D -> [Point4D]
neighbors2 (x, y, z, w) = do
  xo <- [-1, 0, 1]
  yo <- [-1, 0, 1]
  zo <- [-1, 0, 1]
  wo <- [-1, 0, 1]
  guard $ (xo, yo, zo, wo) /= (0, 0, 0, 0)
  return (x + xo, y + yo, z + zo, w + wo)

liveNeighbors2 :: Point4D -> Set Point4D -> [Point4D]
liveNeighbors2 p s = do
  n <- neighbors2 p
  guard $ S.member n s
  return n

allPoints2 :: Set Point4D -> [Point4D]
allPoints2 s = do
  x <- [minX .. maxX]
  y <- [minY .. maxY]
  z <- [minZ .. maxZ]
  w <- [minW .. maxW]
  return (x, y, z, w)
  where
    (xs, ys, zs, ws) = unzip4 $ S.toList s
    minX = minimum xs - 1
    maxX = maximum xs + 1
    minY = minimum ys - 1
    maxY = maximum ys + 1
    minZ = minimum zs - 1
    maxZ = maximum zs + 1
    minW = minimum ws - 1
    maxW = maximum ws + 1

nextState2 :: Set Point4D -> Set Point4D
nextState2 s = S.fromList . filter willBeActive $ allPoints2 s
  where
    willBeActive p =
      let liveN = length $ liveNeighbors2 p s
       in if S.member p s then liveN == 3 || liveN == 2 else liveN == 3

part2 :: Set Point -> Int
part2 initialState = S.size $ loop 0 (init4D initialState)
  where
    init4D = S.map (\(x, y, z) -> (x, y, z, 0))
    loop n state
      | n == 6 = state
      | otherwise = loop (n + 1) $ nextState2 state

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let initialState = readInput input
  -- print initialState
  putStrLn $ "Part 1:\n" <> show (part1 initialState)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 initialState)
