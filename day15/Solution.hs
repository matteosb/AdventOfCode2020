#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package unordered-containers

{-# LANGUAGE BangPatterns #-}

import qualified Data.HashMap.Strict as M

nextNumber :: Int -> Int -> M.HashMap Int Int -> Int
nextNumber prev idx cache = maybe 0 (idx -) $ M.lookup prev cache

makeInitialCache :: [Int] -> M.HashMap Int Int
makeInitialCache initial = M.fromList $ zip (take (length initial - 1) initial) [0 ..]

solve :: [Int] -> Int -> Int
solve initial iters = go (last initial) (length initial) (makeInitialCache initial)
  where
    go !x !l !cache
      | iters == l = x
      | otherwise =
        let idx = l - 1
            next = nextNumber x idx cache
         in go next (l + 1) (M.insert x idx cache)

main :: IO ()
main = do
  let initialNumbers = [19, 20, 14, 0, 9, 1]
  putStrLn $ "Part 1:\n" <> show (solve initialNumbers 2020)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (solve initialNumbers 30000000)
