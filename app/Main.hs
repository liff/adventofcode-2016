{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment (getArgs)
import System.IO

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5


main :: IO ()
main = do
  getArgs >>= \case
    ["day1"] -> Day1.main
    ["day2"] -> Day2.main
    ["day3"] -> Day3.main
    ["day4"] -> Day4.main
    ["day5"] -> Day5.main
    args -> do
      hPutStrLn stderr $ "Unimplemented " ++ show args
  return ()

