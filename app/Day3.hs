module Day3 where

import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

trianglesP :: Parser [(Int, Int, Int)]
trianglesP = triangleP `endBy` newline
  where
    triangleP = (,,) <$> side <*> side <*> side
    side = space *> (fromIntegral <$> integer)

isPossible (s1, s2, s3) = (s1 + s2) > s3 && (s1 + s3) > s2 && (s2 + s3) > s1

countPossible :: [(Int, Int, Int)] -> Int
countPossible = length . filter isPossible

regroup :: [(t, t, t)] -> [(t, t, t)]
regroup ((t11,t21,t31):(t12,t22,t32):(t13,t23,t33):rest) =
  [(t11,t12,t13),(t21,t22,t23),(t31,t32,t33)] ++ regroup rest
regroup _ = []

part1 :: [(Int, Int, Int)] -> Int
part1 = countPossible

part2 :: [(Int, Int, Int)] -> Int
part2 = countPossible . regroup

main :: IO ()
main = do
  let inputFile = "input/day3.txt"
  input <- readFile inputFile
  let Right triangles = parse trianglesP inputFile input
  print $ part1 triangles
  print $ part2 triangles
