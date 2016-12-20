module Day10 where

import           Control.Monad              (forM)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.IntMap                (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer      hiding (space)
import           Text.Megaparsec.String

type Ctx = StateT (Map Receive Int) (Reader (IntMap [Receive]))

data Receive
  = Const !Int
  | Lo !Int
  | Hi !Int
  deriving (Eq, Ord, Show)

insP :: Parser [(Int, [Receive])]
insP = concat <$> ((initIns <|> giveIns) `endBy` newline)
  where
    initIns = do
      _ <- string "value "
      v <- int
      _ <- string " goes to bot "
      d <- int
      return [(d, [Const v])]
    giveIns = do
      _ <- string "bot "
      s <- int
      _ <- string " gives low to "
      l <- container
      _ <- string " and high to "
      h <- container
      return [(l, [Lo s]), (h, [Hi s])]
    container = toBot <|> toOut
    int = fromIntegral <$> integer
    toBot = string "bot " *> int
    toOut = string "output " *> int *> pure (-1)

sources :: Int -> Ctx (Receive, Receive)
sources bot = do
  instructions <- ask
  case IntMap.lookup bot instructions of
    Just [s1, s2] -> return (s1, s2)
    _             -> error "Need two sources"

follow :: Receive -> Ctx Int
follow r@(Lo x) = do
  memo <- get
  case Map.lookup r memo of
    Just known -> return known
    Nothing -> do
      (r1, r2) <- sources x
      res <- min <$> follow r1 <*> follow r2
      modify $ Map.insert r res
      return res
follow r@(Hi x) = do
  memo <- get
  case Map.lookup r memo of
    Just known -> return known
    Nothing -> do
      (r1, r2) <- sources x
      res <- max <$> follow r1 <*> follow r2
      modify $ Map.insert r res
      return res
follow (Const c) = return c

chipsHeldBy :: Int -> Ctx (Int, Int)
chipsHeldBy bot = do
  (sa, sb) <- sources bot
  fa <- follow sa
  fb <- follow sb
  return (min fa fb, max fa fb)

part1 :: IntMap [Receive] -> Int
part1 instructions =
  let memo = Map.empty
      run action = runReader (evalStateT action memo) instructions
      holding = run $ forM [0 .. 209] (\i -> (,) <$> return i <*> chipsHeldBy i)
  in fst $ fromJust $ find (\(_, (l, h)) -> l == 17 && h == 61) holding

main :: IO ()
main = do
  let inputFile = "input/day10.txt"
  input <- readFile inputFile
  let Right instructionList = parse insP inputFile input
      instructions = IntMap.fromListWith (++) instructionList
  print $ part1 instructions
