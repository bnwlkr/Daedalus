module Main where

import System.Random

import View
import Maze
import Stack

main = do
    seed  <- getStdGen
    let rs =  (randomRs (0,3) seed :: [Int])
    let maze = generate  4 4 rs
    view 
    return maze
