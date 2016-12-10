{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Day7 where

import           Data.Either
import           Text.Megaparsec
import           Text.Megaparsec.String

type Ipv7 = ([String], [String])

containsAbba
  :: Eq a
  => [a] -> Bool
containsAbba (a:b:c:d:rest) = a == d && b == c && a /= b || containsAbba (b : c : d : rest)
containsAbba _ = False

ipv7P :: Parser Ipv7
ipv7P = partitionEithers <$> some (hypernet <|> normal)
  where
    normal = Left <$> some lowerChar
    hypernet = Right <$> (char '[' *> some lowerChar <* char ']')

supportsTls :: Ipv7 -> Bool
supportsTls (regular, hypernet) = any containsAbba regular && all (not . containsAbba) hypernet

main :: IO ()
main = do
  let input = "input/day7.txt"
  Right ips <- parse (ipv7P `endBy` newline) input <$> readFile input
  print $ length $ filter supportsTls ips
