module Day3 where

import           Text.Megaparsec        hiding (Pos)
import           Text.Megaparsec.Lexer  hiding (space)
import           Text.Megaparsec.String

trianglesP :: Parser [(Int, Int, Int)]
trianglesP = triangleP `endBy` newline
  where
    triangleP = (,,) <$> side <*> side <*> side
    side = space *> (fromIntegral <$> integer)

isPossible (s1,s2,s3) = (s1 + s2) > s3 && (s1 + s3) > s2 && (s2 + s3) > s1

main :: IO ()
main = do
    let input = "input/day3.txt"
    Right triangles <- parse trianglesP input <$> readFile input
    print $ length $ filter isPossible triangles
