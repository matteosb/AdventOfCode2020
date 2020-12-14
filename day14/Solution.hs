#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package containers,split

{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Data.List.Split
import qualified Data.Map as M

type Bin36 = [Int]

type Mask = [Maybe Int]

data Line = Mem Int Bin36 | NewMask Mask deriving (Show)

parseLine :: String -> Line
parseLine s
  | "mask" == take 4 s = NewMask (readMask (drop 7 s))
  | "mem" == take 3 s = readMem s
  | otherwise = error "invalid line"

readMask :: String -> Mask
readMask =
  map
    ( \case
        'X' -> Nothing
        '1' -> Just 1
        '0' -> Just 0
    )

readMem :: String -> Line
readMem s = Mem addr value
  where
    addr = read $ splitOneOf ['[', ']'] s !! 1
    value = toBin36 . read $ splitOn " = " s !! 1

zeros :: [Int]
zeros = repeat 0

toBin36 :: Int -> Bin36
toBin36 = pad . reverse . go
  where
    go 0 = [0]
    go n = mod n 2 : go (div n 2)
    pad l = take (36 - length l) zeros ++ l

fromBin36 :: Bin36 -> Int
fromBin36 xs = sum $ zipWith (\e x -> x * 2 ^ e) [0 ..] (reverse $ dropWhile (== 0) xs)

applyMask :: Mask -> Bin36 -> Bin36
applyMask = zipWith zipper
  where
    zipper Nothing x = x
    zipper (Just x) _ = x

eval1 :: [Line] -> M.Map Int Bin36
eval1 prog = go prog (error "expected first line to be a mask") M.empty
  where
    go [] _ memory = memory
    go (l : ls) mask memory = case l of
      NewMask newMask -> go ls newMask memory
      Mem addr value -> go ls mask (M.insert addr (applyMask mask value) memory)

sumMemory :: M.Map Int Bin36 -> Int
sumMemory = M.foldl' (\acc x -> acc + fromBin36 x) 0

part1 :: [Line] -> String
part1 prog = show . sumMemory $ eval1 prog

applyMask2M :: Bin36 -> Mask -> [Bin36]
applyMask2M =
  zipWithM
    ( \a m -> case m of
        Nothing -> [0, 1]
        Just x ->
          if x == 0
            then [a]
            else [1]
    )

eval2 :: [Line] -> M.Map Int Bin36
eval2 prog = go prog (error "expected first line to be a mask") M.empty
  where
    go [] _ memory = memory
    go (l : ls) mask memory = case l of
      NewMask newMask -> go ls newMask memory
      Mem addr value -> go ls mask (newMem addr mask value <> memory)
    newMem :: Int -> Mask -> Bin36 -> M.Map Int Bin36
    newMem addr mask value =
      let addrBin36 = toBin36 addr
          maskedAddrs = applyMask2M addrBin36 mask
       in M.fromList $ map (\a -> (fromBin36 a, value)) maskedAddrs

part2 :: [Line] -> String
part2 prog = show . sumMemory $ eval2 prog

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let prog = map parseLine (lines input)
  putStrLn $ "Part 1:\n" <> part1 prog
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> part2 prog
