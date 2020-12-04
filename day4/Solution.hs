#!/usr/bin/env stack
-- stack --resolver lts-16.23 script --package rio

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import RIO.Char (isDigit)
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

requiredFields :: S.Set Text
requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

hasRequiredKeys :: M.Map Text Text -> Bool
hasRequiredKeys m = S.isSubsetOf requiredFields $ M.keysSet m

splitPassports :: Text -> [Text]
splitPassports = TP.splitOn "\n\n"

parsePassport :: Text -> M.Map Text Text
parsePassport t = M.fromList $ map parseField (T.words t)
  where
    parseField a = let (key, val) = TP.breakOn ":" a in (key, T.drop 1 val)

readPassports :: String -> IO [M.Map Text Text]
readPassports fp = map parsePassport . splitPassports <$> readFileUtf8 fp

part1 :: [M.Map Text Text] -> Int
part1 = length . filter hasRequiredKeys

validateIntRange :: Int -> Int -> Text -> Bool
validateIntRange min max v =
  Just True == do
    b <- readMaybe (T.unpack v)
    return $ min <= b && b <= max

validateEntry :: Text -> Text -> Bool
validateEntry "byr" v = validateIntRange 1920 2002 v
validateEntry "iyr" v = validateIntRange 2010 2020 v
validateEntry "eyr" v = validateIntRange 2020 2030 v
validateEntry "hgt" v
  | T.isSuffixOf "in" v = validateIntRange 59 76 $ T.dropEnd 2 v
  | T.isSuffixOf "cm" v = validateIntRange 150 193 $ T.dropEnd 2 v
  | otherwise = False
validateEntry "hcl" v = T.isPrefixOf "#" v && T.all isValidHex (T.drop 1 v)
  where
    validHexChars = S.fromList $ ['0' .. '9'] ++ ['a' .. 'f']
    isValidHex c = S.member c validHexChars
validateEntry "ecl" v = S.member v validColors
  where
    validColors = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateEntry "pid" v = T.length v == 9 && T.all isDigit v
validateEntry _ _ = True

validatePassport :: M.Map Text Text -> Bool
validatePassport p = all (uncurry validateEntry) $ M.toList p

part2 :: [M.Map Text Text] -> Int
part2 = length . filter validatePassport . filter hasRequiredKeys

main :: IO ()
main = runSimpleApp $ do
  passports <- liftIO $ readPassports "./input.txt"
  logInfo "Part 1:"
  logInfo . display $ part1 passports
  logInfo "---------------------------------------------"
  logInfo "Part 2:"
  logInfo . display $ part2 passports
