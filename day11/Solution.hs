#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package vector

import Control.Monad
import Data.List
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

type SeatingChart = Vector (Vector Char)

type Coord = (Int, Int)

type NeighborsFunc = Coord -> BoundsCheck -> SeatingChart -> Int

type BoundsCheck = (Int, Int) -> Bool

makeChart :: String -> SeatingChart
makeChart = V.fromList . map V.fromList . lines

nextChart :: NeighborsFunc -> Int -> SeatingChart -> SeatingChart
nextChart neighborsFunction occupiedThreshold sc = V.imap mapRow sc
  where
    inBounds = makeBoundsCheck sc
    mapRow y r = V.imap (mapCol y) r
    mapCol y x val =
      let oc = neighborsFunction (x, y) inBounds sc
       in case val of
            '.' -> '.'
            '#' -> if oc >= occupiedThreshold then 'L' else '#'
            'L' -> if oc == 0 then '#' else 'L'
            _ -> error $ "Unexpected char: " ++ [val]

findSteadyState :: NeighborsFunc -> Int -> SeatingChart -> SeatingChart
findSteadyState fn thr sc =
  let next = nextChart fn thr sc
   in if next == sc then next else findSteadyState fn thr next

countOccupied :: SeatingChart -> Int
countOccupied = V.foldl' (V.foldl' sumCells) 0
  where
    sumCells acc c = acc + (if c == '#' then 1 else 0)

makeBoundsCheck :: SeatingChart -> BoundsCheck
makeBoundsCheck sc = inBounds
  where
    maxX = V.length $ sc ! 1
    maxY = V.length sc
    inBounds (a, b) = (a < maxX) && (b < maxY) && (a >= 0) && (b >= 0)

part1Neighbors :: NeighborsFunc
part1Neighbors coord inBounds sc = length . filter isOccupied $ neighbors coord
  where
    isOccupied (x, y) = ((sc ! y) ! x) == '#'
    neighbors (x, y) = do
      xo <- [-1, 0, 1]
      yo <- [-1, 0, 1]
      let c = (x + xo, y + yo)
      guard $ inBounds c && c /= (x, y)
      return c

part1 :: SeatingChart -> Int
part1 initChart = countOccupied (findSteadyState part1Neighbors 4 initChart)

unfolds :: BoundsCheck -> SeatingChart -> [Coord -> Maybe (Char, Coord)]
unfolds inBounds sc = do
  xo <- [-1, 0, 1]
  yo <- [-1, 0, 1]
  guard $ (xo, yo) /= (0, 0)
  return $ mkUnfold xo yo
  where
    get (x, y) = (sc ! y) ! x
    mkUnfold xo yo (x, y) =
      let c = (x + xo, y + yo)
       in if inBounds c then Just (get c, c) else Nothing

part2Neighbors :: NeighborsFunc
part2Neighbors coord inBounds sc = length . filter containsOccupiedSeat $ linesOfSight
  where
    linesOfSight = map (`unfoldr` coord) $ unfolds inBounds sc
    firstSeat = find (/= '.')
    containsOccupiedSeat n = Just '#' == firstSeat n

part2 :: SeatingChart -> Int
part2 initChart = countOccupied (findSteadyState part2Neighbors 5 initChart)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let initChart = makeChart input
  putStrLn $ "Part 1:\n" <> show (part1 initChart)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 initChart)
