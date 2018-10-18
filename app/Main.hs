module Main where

import System.Random

import Maze

main = do
    seed  <- getStdGen
    let rs =  (randomRs (0,3) seed :: [Int])
    --let maze = generate $ blank w h $ rs $
    return rs
