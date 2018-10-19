module Main where

import System.Random
import System.Environment

import View
import Maze
import Stack

main = do
    args <- getArgs
    let width = read (args !! 0) :: Int
    let height = read (args !! 1) :: Int
    seed  <- getStdGen
    let rs =  (randomRs (0,2) seed :: [Int])
    let maze = generate width height rs
    view maze
    print "done"
