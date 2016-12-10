module Day6 where

import           Data.List              (group, maximumBy, sort, transpose)
import           Data.Ord
import           Text.Megaparsec
import           Text.Megaparsec.String

messagesP :: Parser [String]
messagesP = many lowerChar `endBy` newline

main :: IO ()
main = do
  let input = "input/day6.txt"
  Right messages <- parse messagesP input <$> readFile input
  putStrLn $ map (head . maximumBy (comparing length) . group . sort) $ transpose messages
