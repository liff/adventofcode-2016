module Day4 where

import           Data.Char              (chr, ord)
import           Data.List              (find, group, intercalate, sort, sortBy)
import           Data.Ord
import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

data Room =
  Room [Char]
       Int
       [Char]
  deriving (Show)

name :: Room -> String
name (Room n _ _) = n

sector :: Room -> Int
sector (Room _ s _) = s

roomsP :: Parser [Room]
roomsP = roomP `endBy` newline
  where
    roomP = Room <$> nameP <*> sect <*> checksum
    nameP = intercalate "-" <$> many lowerChar `endBy` char '-'
    sect = fromIntegral <$> integer
    checksum = char '[' *> many lowerChar <* char ']'

isReal :: Room -> Bool
isReal (Room name _ checksum) =
  map head (take 5 $ sortBy ordr $ group $ sort $ filter (/= '-') name) == checksum
  where
    ordr b a
      | comparing length a b == EQ = comparing head b a
      | otherwise = comparing length a b

part1 :: [Room] -> Int
part1 = sum . map sector . filter isReal

rot :: Int -> Char -> Char
rot _ '-' = ' '
rot n c = chr (a + ((ord c - a + n) `mod` (z - a + 1)))
  where
    a = ord 'a'
    z = ord 'z'

part2 :: [Room] -> Maybe Int
part2 = fmap sector . find (\r -> name r == "northpole object storage") . decrypt
  where
    decrypt = map (\(Room n s c) -> Room (map (rot s) n) s c)

main :: IO ()
main = do
  let inputFile = "input/day4.txt"
  input <- readFile inputFile
  let Right rooms = parse roomsP inputFile input
  print $ part1 rooms
  mapM_ print $ part2 rooms
