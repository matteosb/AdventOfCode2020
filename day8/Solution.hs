#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package containers

{-# LANGUAGE MultiWayIf #-}

import Data.List
import qualified Data.Set as S

data Op = Nop Int | Acc Int | Jmp Int deriving (Show)

data TerminationReason = Normal | InfiniteLoop | Error deriving (Show, Eq)

-- TODO: simplify constructor
parseLine :: String -> Op
parseLine s = case words s of
  "nop" : i : _ -> Nop $ readInt i
  "acc" : i : _ -> Acc $ readInt i
  "jmp" : i : _ -> Jmp $ readInt i
  _ -> error "failed to parse line"
  where
    readInt x = read (dropWhile (== '+') x) :: Int

eval :: [Op] -> (Int, TerminationReason)
eval prog = eval' prog 0 0 S.empty

eval' :: [Op] -> Int -> Int -> S.Set Int -> (Int, TerminationReason)
eval' prog inst acc visited =
  if
      | S.member inst visited -> (acc, InfiniteLoop)
      | inst == progLen -> (acc, Normal)
      | inst > progLen -> (acc, Error)
      | otherwise ->
        case prog !! inst of
          Nop _ -> eval' prog (inst + 1) acc visited'
          Acc n -> eval' prog (inst + 1) (acc + n) visited'
          Jmp n -> eval' prog (inst + n) acc visited'
  where
    visited' = S.insert inst visited
    progLen = length prog

part1 :: [Op] -> Int
part1 program = fst $ eval program

possiblePrograms :: [Op] -> [[Op]]
possiblePrograms prog = go prog [] [prog]
  where
    go :: [Op] -> [Op] -> [[Op]] -> [[Op]]
    go [] _ res = res
    go (x : xs) ys res = case x of
      Acc _ -> go xs (x : ys) res
      Nop i -> go xs (x : ys) ((reverse ys ++ (Jmp i : xs)) : res)
      Jmp i -> go xs (x : ys) ((reverse ys ++ (Nop i : xs)) : res)

part2 :: [Op] -> Maybe (Int, TerminationReason)
part2 prog = find (\(_, r) -> r == Normal) $ map eval $ possiblePrograms prog

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let program = map parseLine $ filter (not . null) $ lines input
  putStrLn $ "Part 1:\n" <> show (part1 program)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 program)
