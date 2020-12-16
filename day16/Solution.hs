#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package split,parsec,containers

import Control.Monad
import Data.Either
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec

type Ticket = [Int]

data Rule = Rule String [S.Set Int] deriving (Show)

parseTicket :: String -> Ticket
parseTicket s = read $ "[" <> s <> "]"

parseRule :: String -> Rule
parseRule s = fromRight (error "no parse") $ parse rule "input" s
  where
    range = (\l u -> S.fromList [read l .. read u]) <$> many1 digit <* char '-' <*> many1 digit
    rule = Rule <$> many (letter <|> space) <* string ": " <*> range `sepBy` string " or "

readInput :: String -> ([Rule], Ticket, [Ticket])
readInput s = (rules, ourTicket, nearbyTickets)
  where
    sections = map lines $ splitOn "\n\n" s
    rules = map parseRule $ head sections
    ourTicket = parseTicket $ sections !! 1 !! 1
    nearbyTickets = map parseTicket . tail $ sections !! 2

unionAll :: [S.Set Int] -> S.Set Int
unionAll = foldl1 S.union

part1 :: [Rule] -> [Ticket] -> Int
part1 rules nearbyTickets = sum $ do
  ticket <- nearbyTickets
  field <- ticket
  guard . not . isValid $ field
  return field
  where
    isValid f = S.member f validSet
    validSet = unionAll $ rules >>= (\(Rule _ bounds) -> bounds)

validTickets :: [Rule] -> [Ticket] -> [Ticket]
validTickets rules nearbyTickets = do
  ticket <- nearbyTickets
  guard $ all isValid ticket
  return ticket
  where
    isValid f = S.member f validSet
    validSet = unionAll $ rules >>= (\(Rule _ bounds) -> bounds)

matchingRuleNames :: [Rule] -> [Ticket] -> Int -> S.Set String
matchingRuleNames rules tickets col = S.fromList . map ruleName . filter filt $ rules
  where
    colValues = map (!! col) tickets
    filt (Rule _ bounds) = let bs = unionAll bounds in all (`S.member` bs) colValues
    ruleName (Rule n _) = n

matchingRuleNamesByCol :: [Rule] -> [Ticket] -> M.Map Int (S.Set String)
matchingRuleNamesByCol r ts = M.fromList . zip [0 ..] $ map (matchingRuleNames r ts) colRange
  where
    numCols = length . head $ ts
    colRange = [0 .. (numCols - 1)]

matchRulesToCols :: [Rule] -> [Ticket] -> M.Map String Int
matchRulesToCols rs ts = invert $ loop M.empty (matchingRuleNamesByCol rs ts)
  where
    invert :: M.Map Int String -> M.Map String Int
    invert = M.fromList . map (\(x, y) -> (y, x)) . M.toList
    loop matched unmatched
      | M.size matched == length rs = matched
      | otherwise = loop (newMatches <> matched) newUnmatched
      where
        (withMatch, withoutMatch) = M.partition (\l -> S.size l == 1) unmatched
        newMatches = M.map (S.elemAt 0) withMatch
        matchedSet = S.fromList $ M.elems newMatches
        newUnmatched = M.map (`S.difference` matchedSet) withoutMatch

part2 :: [Rule] -> Ticket -> [Ticket] -> Int
part2 rules ourTicket nearbyTickets = product $ M.map (ourTicket !!) depRules
  where
    rulesToCols = matchRulesToCols rules (validTickets rules nearbyTickets)
    depRules = M.filterWithKey (\k _ -> "departure" `isPrefixOf` k) rulesToCols

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let (rules, ourTicket, nearbyTickets) = readInput input
  print $ matchRulesToCols rules (validTickets rules nearbyTickets)
  putStrLn $ "Part 1:\n" <> show (part1 rules nearbyTickets)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (part2 rules ourTicket nearbyTickets)
