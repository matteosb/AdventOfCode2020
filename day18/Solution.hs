#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package parsec

import Data.Either
import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)

data Term = Paren Expr | Num Int deriving (Show)

data Operator = Mult | Add deriving (Show)

data Expr = BinaryOperation Term Operator Expr | Null Term deriving (Show)

type PrecedenceFun = Operator -> Operator -> Bool

term, num, paren :: Parser Term
term = num <|> paren
paren = Paren <$> between (char '(') (char ')') expr
num = Num . read <$> many1 digit

operator :: Parser Operator
operator = try plus <|> star
  where
    plus = Add <$ (optional space *> string "+" <* optional space)
    star = Mult <$ (optional space *> string "*" <* optional space)

expr :: Parser Expr
expr = do
  l <- term
  r <- optionMaybe $ BinaryOperation l <$> operator <*> expr
  return $ fromMaybe (Null l) r

evalT :: Term -> PrecedenceFun -> Int
evalT (Num i) _ = i
evalT (Paren e) precedence = eval e precedence

eval :: Expr -> PrecedenceFun -> Int
eval (BinaryOperation t o r) p =
  case r of
    BinaryOperation t' o' r' ->
      if p o' o
        then evalT t p * eval r p
        else eval (BinaryOperation (Num (func (evalT t p) (evalT t' p))) o' r') p
    Null t' -> func (evalT t p) (evalT t' p)
  where
    func = case o of
      Add -> (+)
      Mult -> (*)
eval (Null t) precedence = evalT t precedence

evalSum :: PrecedenceFun -> [Expr] -> Int
evalSum p = sum . map (`eval` p)

part1 :: PrecedenceFun
part1 _ _ = False

part2 :: PrecedenceFun
part2 Add Mult = True
part2 _ _ = False

main :: IO ()
main = do
  input <- readFile "./input.txt"
  -- TODO: figure out why expr `endBy` newline isn't working
  let exprs = map (fromRight (error "no parse") . parse expr "input") (lines input)
  putStrLn $ "Part 1:\n" <> show (evalSum part1 exprs)
  putStrLn "---------------------------------------------"
  putStrLn $ "Part 2:\n" <> show (evalSum part2 exprs)
