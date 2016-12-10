module Day2 where

import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.String

type Pos = (Int, Int)

data Move
  = L
  | U
  | R
  | D
  deriving (Eq, Show)

move :: Pos -> Move -> Pos
move (x, y) L = (max (x - 1) (-1), y)
move (x, y) U = (x, max (y - 1) (-1))
move (x, y) R = (min 1 (x + 1), y)
move (x, y) D = (x, min 1 (y + 1))

push (-1, -1) = '1'
push (0, -1)  = '2'
push (1, -1)  = '3'
push (-1, 0)  = '4'
push (0, 0)   = '5'
push (1, 0)   = '6'
push (-1, 1)  = '7'
push (0, 1)   = '8'
push (1, 1)   = '9'
push _        = undefined

movesP :: Parser [[Move]]
movesP = many moveP `endBy` newline
  where
    moveP :: Parser Move
    moveP = l <|> u <|> r <|> d
    l = char 'L' *> pure L
    u = char 'U' *> pure U
    r = char 'R' *> pure R
    d = char 'D' *> pure D

main :: IO ()
main = do
  let input = "input/day2.txt"
  Right moves <- parse movesP input <$> readFile input
  let start = (0, 0)
  putStrLn $ map push $ tail $ scanl (foldl move) start moves
