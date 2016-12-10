module Day4 where

import           Data.List              (group, sort, sortBy)
import           Data.Ord
import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

data Room =
  Room [Char]
       Int
       [Char]
  deriving (Show)

sector :: Room -> Int
sector (Room _ s _) = s

roomsP :: Parser [Room]
roomsP = roomP `endBy` newline
  where
    roomP = Room <$> name <*> sect <*> checksum
    name = concat <$> many lowerChar `endBy` char '-'
    sect = fromIntegral <$> integer
    checksum = char '[' *> many lowerChar <* char ']'

isReal :: Room -> Bool
isReal (Room name _ checksum) = map head (take 5 $ sortBy ord $ group $ sort name) == checksum
  where
    ord b a
      | comparing length a b == EQ = comparing head b a
      | otherwise = comparing length a b

main :: IO ()
main = do
  let input = "input/day4.txt"
  Right rooms <- parse roomsP input <$> readFile input
  print $ sum $ map sector $ filter isReal rooms
