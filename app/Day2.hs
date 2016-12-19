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

move1 :: Pos -> Move -> Pos
move1 (x, y) L = (max (x - 1) (-1), y)
move1 (x, y) U = (x, max (y - 1) (-1))
move1 (x, y) R = (min 1 (x + 1), y)
move1 (x, y) D = (x, min 1 (y + 1))

push1 (-1, -1) = '1'
push1 (0, -1)  = '2'
push1 (1, -1)  = '3'
push1 (-1, 0)  = '4'
push1 (0, 0)   = '5'
push1 (1, 0)   = '6'
push1 (-1, 1)  = '7'
push1 (0, 1)   = '8'
push1 (1, 1)   = '9'
push1 _        = undefined

move2 :: Pos -> Move -> Pos
move2 (x, y) d =
  let minX = -(2 - abs y)
      maxX = -minX
      minY = -(2 - abs x)
      maxY = -minY
  in case d of
       L -> (max (x - 1) minX, y)
       U -> (x, max (y - 1) minY)
       R -> (min maxX (x + 1), y)
       D -> (x, min maxY (y + 1))

push2 (0, -2)  = '1'
push2 (-1, -1) = '2'
push2 (0, -1)  = '3'
push2 (1, -1)  = '4'
push2 (-2, 0)  = '5'
push2 (-1, 0)  = '6'
push2 (0, 0)   = '7'
push2 (1, 0)   = '8'
push2 (2, 0)   = '9'
push2 (-1, 1)  = 'A'
push2 (0, 1)   = 'B'
push2 (1, 1)   = 'C'
push2 (0, 2)   = 'D'
push2 _        = undefined

movesP :: Parser [[Move]]
movesP = many moveP `endBy` newline
  where
    moveP :: Parser Move
    moveP = l <|> u <|> r <|> d
    l = char 'L' *> pure L
    u = char 'U' *> pure U
    r = char 'R' *> pure R
    d = char 'D' *> pure D

part1 :: [[Move]] -> String
part1 moves =
  let start = (0, 0)
  in map push1 $ tail $ scanl (foldl move1) start moves

part2 :: [[Move]] -> String
part2 moves =
  let start = (-2, 0)
  in map push2 $ tail $ scanl (foldl move2) start moves

main :: IO ()
main = do
  let inputFile = "input/day2.txt"
  input <- readFile inputFile
  let Right moves = parse movesP inputFile input
  putStrLn $ part1 moves
  putStrLn $ part2 moves
