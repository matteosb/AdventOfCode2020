#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package containers

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Text.ParserCombinators.ReadP

data Rule
  = Literal Int Char
  | Subrules Int [Int]
  | Disjunction Int [Int] [Int]
  deriving (Show, Eq)

-- Input parser
digit, ab, newline, alphaNum :: ReadP Char
digit = satisfy (`elem` ['0' .. '9'])
ab = satisfy (\c -> c == 'a' || c == 'b')
newline = char '\n'
alphaNum = digit <|> satisfy (`elem` ['a' .. 'z'])

int :: ReadP Int
int = read <$> many1 digit

ruleNumber :: ReadP Int
ruleNumber = int <* string ": "

charLiteral, subrules, disjunction, rule :: ReadP Rule
charLiteral = Literal <$> ruleNumber <*> between (char '"') (char '"') ab
subrules = Subrules <$> ruleNumber <*> int `sepBy` char ' '
disjunction = Disjunction <$> ruleNumber <*> (int `endBy` char ' ') <* char '|' <* char ' ' <*> int `sepBy` char ' '
rule = charLiteral <|> disjunction <|> subrules

inputParser :: ReadP ([Rule], [String])
inputParser = (,) <$> rule `endBy` newline <* newline <*> many1 alphaNum `endBy` newline

-- Message parser
mkRuleMap :: [Rule] -> M.Map Int Rule
mkRuleMap = M.fromList . map kv
  where
    kv r = case r of
      (Literal i _) -> (i, r)
      (Subrules i _) -> (i, r)
      (Disjunction i _ _) -> (i, r)

messageParser :: M.Map Int Rule -> Int -> ReadP ()
messageParser m rn = do
  let Just currentRule = M.lookup rn m
  case currentRule of
    Literal _ c -> () <$ char c
    Subrules _ sr -> mapM_ (messageParser m) sr
    Disjunction _ lhs rhs -> forM_ lhs (messageParser m) <|> forM_ rhs (messageParser m)
  when (rn == 0) eof

matches :: ReadP () -> String -> Bool
matches parser str = [((), "")] == readP_to_S parser str

main :: IO ()
main = do
  input <- readFile "./input.txt"
  let ((rules, messages), "") = last $ readP_to_S inputParser input
      msgParser = messageParser (mkRuleMap rules) 0
  -- Just change input.txt to compute parts 1 and 2
  print . length $ filter (matches msgParser) messages
