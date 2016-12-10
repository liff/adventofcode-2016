module Day1 where

import           Control.Applicative
import           System.IO              (readFile)
import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.Lexer
import           Text.Megaparsec.String

data Turn
  = L
  | R
  deriving (Eq, Show)

data Step =
  Step Turn
       Int
  deriving (Eq, Show)

data Orientation
  = N
  | E
  | S
  | W
  deriving (Eq, Show)

type Pos = (Int, Int)

data Placement =
  Placement Orientation
            Pos
  deriving (Eq, Show)

distance :: Pos -> Pos -> Int
distance (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

stepsP :: Parser [Step]
stepsP = stepP `sepBy1` string ", "
  where
    stepP = Step <$> (l <|> r) <*> n
    l = char 'L' *> pure L
    r = char 'R' *> pure R
    n = fromIntegral <$> integer

turn :: Turn -> Orientation -> Orientation
turn R N = E
turn R E = S
turn R S = W
turn R W = N
turn L N = W
turn L W = S
turn L S = E
turn L E = N

move :: Orientation -> Int -> Pos -> Pos
move N n (x, y) = (x, y - n)
move E n (x, y) = (x + n, y)
move S n (x, y) = (x, y + n)
move W n (x, y) = (x - n, y)

step :: Placement -> Step -> Placement
step (Placement o pos) (Step t n) =
  let o' = turn t o
      pos' = move o' n pos
  in Placement o' pos'

main :: IO ()
main = do
  let input = "input/day1.txt"
  Right steps <- parse stepsP input <$> readFile input
  let start@(Placement _ startPos) = Placement N (0, 0)
      (Placement _ endPos) = foldl step start steps
  print $ distance startPos endPos
