module Day6 where

import           Data.List              (group, maximumBy, minimumBy, sort,
                                         transpose)
import           Data.Ord
import           Text.Megaparsec
import           Text.Megaparsec.String

messagesP :: Parser [String]
messagesP = many lowerChar `endBy` newline

part1 :: [String] -> String
part1 = map (head . maximumBy (comparing length) . group . sort) . transpose

part2 :: [String] -> String
part2 = map (head . minimumBy (comparing length) . group . sort) . transpose

main :: IO ()
main = do
  let inputFile = "input/day6.txt"
  input <- readFile inputFile
  let Right messages = parse messagesP inputFile input
  putStrLn $ part1 messages
  putStrLn $ part2 messages
