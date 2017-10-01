module Main where

import System.Environment

import Phoityne.Example.Math
import Starman

main :: IO ()
main = do
  args <- getArgs
  let word = args!!0
  let n = read (args!!1) :: Int
  starman word n