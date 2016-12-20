module Day9 where

import           Debug.Trace            (trace)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

marker1 :: Parser Int
marker1 = do
  _ <- char '('
  numChars <- fromIntegral <$> integer
  _ <- char 'x'
  repCount <- fromIntegral <$> integer
  _ <- char ')'
  _ <- count numChars anyChar
  return $ repCount * numChars

marker2 :: Parser Int
marker2 = do
  _ <- char '('
  numChars <- fromIntegral <$> integer
  _ <- char 'x'
  repCount <- fromIntegral <$> integer
  _ <- char ')'
  payload <- count numChars anyChar
  let Right decodedPayload = parse decoder2 "foo" payload
  return $ repCount * decodedPayload

bulk1 :: Parser Int
bulk1 = length <$> some (noneOf "(")

decoder1 :: Parser Int
decoder1 = sum <$> some (marker1 <|> bulk1)

decoder2 :: Parser Int
decoder2 = sum <$> some (marker2 <|> bulk1)

main :: IO ()
main = do
  let inputFile = "input/day9.txt"
  input <- filter (`notElem` " \t\r\n") <$> readFile inputFile
  let Right part1 = parse decoder1 inputFile input
  print part1
  let Right part2 = parse decoder2 inputFile input
  print part2
