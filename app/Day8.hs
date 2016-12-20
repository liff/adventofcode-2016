module Day8 where

import           Data.Array.Repa        ((:.) (..), All (..), Any (..))
import qualified Data.Array.Repa        as Repa
import           Data.List              (intercalate)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

data Ins
  = Rect Int
         Int
  | RotR Int
         Int
  | RotD Int
         Int
  deriving (Show)

type Display = Repa.Array Repa.D Repa.DIM2 Bool

insP :: Parser Ins
insP = rect <|> rotR <|> rotD
  where
    rect = Rect <$> (string "rect " *> int <* char 'x') <*> int
    rotR = RotR <$> (string "rotate row y=" *> int) <*> (string " by " *> int)
    rotD = RotD <$> (string "rotate column x=" *> int) <*> (string " by " *> int)
    int = fromInteger <$> integer -- :: Parser Int

toPixel :: Bool -> Char
toPixel False = ' '
toPixel True  = '#'

apply :: Display -> Ins -> Display
apply disp (Rect cols rows) = Repa.traverse disp id draw
  where
    draw at sh@(Repa.Z :. r :. c)
      | r < rows && c < cols = True
      | otherwise = at sh
apply disp (RotR row n) = Repa.traverse disp id rot
  where
    (Repa.Z :. _ :. width) = Repa.extent disp
    rot at sh@(Repa.Z :. r :. c)
      | r == row = at (Repa.Z :. r :. (c - n) `mod` width)
      | otherwise = at sh
apply disp (RotD col n) = Repa.traverse disp id rot
  where
    (Repa.Z :. height :. _) = Repa.extent disp
    rot at sh@(Repa.Z :. r :. c)
      | c == col = at (Repa.Z :. (r - n) `mod` height :. c)
      | otherwise = at sh

render :: Display -> [String]
render disp = map renderRow [0 .. height]
  where
    (Repa.Z :. height :. _) = Repa.extent disp
    renderRow row = Repa.toList $ Repa.map toPixel $ Repa.slice disp (Any :. (row :: Int) :. All)

part1 :: Display -> Int
part1 = length . filter id . Repa.toList

part2 :: Display -> String
part2 = intercalate "\n" . render

main :: IO ()
main = do
  let inputFile = "input/day8.txt"
  input <- readFile inputFile
  let Right instructions = parse (insP `endBy` newline) inputFile input
  let blank = Repa.fromFunction (Repa.ix2 6 50) (const False)
      display = foldl apply blank instructions
  print $ part1 display
  putStrLn $ part2 display
