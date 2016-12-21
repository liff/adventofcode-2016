module Day10 where

import           Control.Monad              (forM)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, fromMaybe)
import           Text.Megaparsec
import           Text.Megaparsec.Lexer      hiding (space)
import           Text.Megaparsec.String

data Recipient
  = Bot !Int
  | Out !Int
  deriving (Eq, Ord, Show)

data Receive
  = Const !Int
  | Lo !Int
  | Hi !Int
  deriving (Eq, Ord, Show)

type Ctx = StateT (Map Receive Int) (Reader (Map Recipient [Receive]))

insP :: Parser (Map Recipient [Receive])
insP = Map.fromListWith (++) . concat <$> ((initIns <|> giveIns) `endBy` newline)
  where
    initIns = do
      _ <- string "value "
      v <- int
      _ <- string " goes to bot "
      d <- int
      return [(Bot d, [Const v])]
    giveIns = do
      _ <- string "bot "
      s <- int
      _ <- string " gives low to "
      l <- recipient
      _ <- string " and high to "
      h <- recipient
      return [(l, [Lo s]), (h, [Hi s])]
    recipient = toBot <|> toOut
    int = fromIntegral <$> integer
    toBot = Bot <$> (string "bot " *> int)
    toOut = Out <$> (string "output " *> int)

sources :: Recipient -> Ctx [Receive]
sources recipient = do
  instructions <- ask
  return $ fromMaybe [] (Map.lookup recipient instructions)

memoize :: (Receive -> Ctx Int) -> Receive -> Ctx Int
memoize f r = do
  memo <- get
  case Map.lookup r memo of
    Just known -> return known
    Nothing -> do
      res <- f r
      modify $ Map.insert r res
      return res

follow :: Receive -> Ctx Int
follow = memoize follow'
  where
    follow' (Lo x) = do
      ss <- sources (Bot x)
      case ss of
        [r1, r2] -> min <$> follow r1 <*> follow r2
    follow' (Hi x) = do
      ss <- sources (Bot x)
      case ss of
        [r1, r2] -> max <$> follow r1 <*> follow r2
    follow' (Const c) = return c

chipsHeldBy :: Int -> Ctx (Int, Int)
chipsHeldBy bot = do
  [sa, sb] <- sources (Bot bot)
  fa <- follow sa
  fb <- follow sb
  return (min fa fb, max fa fb)

part1 :: Map Recipient [Receive] -> Int
part1 instructions =
  let memo = Map.empty
      run action = runReader (evalStateT action memo) instructions
      holding = run $ forM [0 .. 209] (\i -> (,) <$> return i <*> chipsHeldBy i)
  in fst $ fromJust $ find (\(_, (l, h)) -> l == 17 && h == 61) holding

part2 :: Map Recipient [Receive] -> Int
part2 instructions =
  let memo = Map.empty
      run action = runReader (evalStateT action memo) instructions
      out0 = run $ do [sa] <- sources (Out 0)
                      follow sa
      out1 = run $ do [sa] <- sources (Out 1)
                      follow sa
      out2 = run $ do [sa] <- sources (Out 2)
                      follow sa
  in out0 * out1 * out2

main :: IO ()
main = do
  let inputFile = "input/day10.txt"
  input <- readFile inputFile
  let Right instructions = parse insP inputFile input
  print $ part1 instructions
  print $ part2 instructions
