
module Main where

import PBuddy.Simulator

main :: IO ()
main = getContents >>= runSimulator
