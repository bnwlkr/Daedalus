module Main where

import System.Random

import View
import Maze
import Stack

main = do
    seed  <- getStdGen
    let rs =  (randomRs (0,2) seed :: [Int])
    let maze = generate 100 100  rs
    view maze
    print "done"
