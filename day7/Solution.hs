#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package parsec,containers

{-# LANGUAGE TupleSections #-}

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser)

data Bag = Bag String String deriving (Show, Eq, Ord)

data Rule = Rule Bag (M.Map Bag Int) deriving (Show, Eq)

--- Parser
word :: Parser String
word = many1 letter

int :: Parser Int
int = read <$> many1 digit

bag :: Parser Bag
bag = Bag <$> word <* space <*> word <* space <* string "bag" <* optional (char 's')

quantifiedBags :: Parser (M.Map Bag Int)
quantifiedBags = M.fromList <$> sepBy quantifiedBag (string ", ")
  where
    quantifiedBag = flip (,) <$> int <* space <*> bag

noBag :: Parser (M.Map Bag Int)
noBag = M.empty <$ string "no other bags"

rule :: Parser Rule
rule = Rule <$> bag <* string " contain " <*> (noBag <|> quantifiedBags) <* char '.'

-----

part1 :: [Rule] -> Int
part1 rules = S.size $ S.difference (findAncestors seedSet) seedSet
  where
    seedSet = S.singleton (Bag "shiny" "gold")
    insertRule m (Rule parent childMap) =
      let childrenToParent = M.fromList $ map (,S.singleton parent) (M.keys childMap)
       in M.unionWith S.union childrenToParent m
    childToParentMap = foldl' insertRule M.empty rules
    findParents b = M.findWithDefault S.empty b childToParentMap
    findAncestors bgs =
      let newBgs = foldl' S.union bgs $ S.map findParents bgs
       in if newBgs == bgs then bgs else findAncestors newBgs

part2 :: [Rule] -> Int
part2 rules = countChildBags (Bag "shiny" "gold")
  where
    ruleMap = M.fromList $ map (\(Rule parent childMap) -> (parent, childMap)) rules
    countChildBags b =
      if null children
        then 0
        else sum $ map (\(child, num) -> num + num * countChildBags child) children
      where
        children = maybe [] M.toList (M.lookup b ruleMap)

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let (Right rules) = parse (endBy rule newline) "input.txt" input
  putStrLn $ "Part 1:\n" <> show (part1 rules)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 rules)
