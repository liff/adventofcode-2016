{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment (getArgs)
import           System.IO

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10

main :: IO ()
main = do
  getArgs >>=
    \case
      ["day1"] -> Day1.main
      ["day2"] -> Day2.main
      ["day3"] -> Day3.main
      ["day4"] -> Day4.main
      ["day5"] -> Day5.main
      ["day6"] -> Day6.main
      ["day7"] -> Day7.main
      ["day8"] -> Day8.main
      ["day9"] -> Day9.main
      ["day10"] -> Day10.main
      args -> hPutStrLn stderr $ "Unimplemented " ++ show args
  return ()
