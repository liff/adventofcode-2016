module Day1 where

import           Control.Applicative
import           Data.Maybe             (fromMaybe)
import           System.IO              (readFile)
import           Text.Megaparsec        hiding (Pos, between)
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

between :: Pos -> Pos -> [Pos]
between (sx, sy) (ex, ey)
  | sx == ex =
    [ (sx, y)
    | y <-
       if sy < ey
         then [sy .. ey - 1]
         else reverse [ey + 1 .. sy] ]
  | sy == ey =
    [ (x, sy)
    | x <-
       if sx < ex
         then [sx .. ex - 1]
         else reverse [ex + 1 .. sx] ]
  | otherwise = []

stepAll :: Placement -> Step -> [Placement]
stepAll (Placement o pos) (Step t n) =
  let o' = turn t o
      allPos =
        [ move o' i pos
        | i <- [1 .. n] ]
  in Placement o' <$> allPos

part1 :: [Step] -> Int
part1 steps =
  let start@(Placement _ startPos) = Placement N (0, 0)
      (Placement _ endPos) = foldl step start steps
  in distance startPos endPos

firstDuplicate
  :: Eq a
  => [a] -> Maybe a
firstDuplicate [] = Nothing
firstDuplicate (a:as)
  | a `elem` as = Just a
  | otherwise = firstDuplicate as

part2 :: [Step] -> Int
part2 steps =
  let start@(Placement _ startPos) = Placement N (0, 0)
      interpolated []         = []
      interpolated (a:b:rest) = between a b ++ interpolated (b : rest)
      interpolated [a]        = [a]
      positions =
        interpolated $ (\(Placement _ pos) -> pos) <$> scanl step start steps
      endPos = fromMaybe startPos (firstDuplicate positions)
  in distance startPos endPos

main :: IO ()
main = do
  let inputFile = "input/day1.txt"
  input <- readFile inputFile
  let (Right steps) = parse stepsP inputFile input
  print $ part1 steps
  print $ part2 steps
