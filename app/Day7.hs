module Day7 where

import           Data.Either
import           Data.Tuple             (swap)
import           Text.Megaparsec
import           Text.Megaparsec.String

type Ipv7 = ([String], [String])

containsAbba
  :: Eq a
  => [a] -> Bool
containsAbba (a:b:c:d:rest) =
  a == d && b == c && a /= b || containsAbba (b : c : d : rest)
containsAbba _ = False

findAbas
  :: Eq a
  => [a] -> [(a, a)]
findAbas (a:b:c:rest) =
  if a == c && a /= b
    then (a, b) : findAbas (b : c : rest)
    else findAbas (b : c : rest)
findAbas _ = []

ipv7P :: Parser Ipv7
ipv7P = partitionEithers <$> some (hypernet <|> normal)
  where
    normal = Left <$> some lowerChar
    hypernet = Right <$> (char '[' *> some lowerChar <* char ']')

supportsTls :: Ipv7 -> Bool
supportsTls (regular, hypernet) =
  any containsAbba regular && all (not . containsAbba) hypernet

supportsSsl :: Ipv7 -> Bool
supportsSsl (regular, hypernet) =
  let babs = concatMap (map swap . findAbas) regular
  in any (`elem` babs) $ concatMap findAbas hypernet

part1 :: [Ipv7] -> Int
part1 = length . filter supportsTls

part2 :: [Ipv7] -> Int
part2 = length . filter supportsSsl

main :: IO ()
main = do
  let inputFile = "input/day7.txt"
  input <- readFile inputFile
  let Right ips = parse (ipv7P `endBy` newline) inputFile input
  print $ part1 ips
  print $ part2 ips
