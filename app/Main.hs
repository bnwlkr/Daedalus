module Main where

import System.Random
import System.Environment
import Data.Time.Clock.POSIX

import View
import Maze
import Stack
import Solve

main = do
    args <- getArgs
    let width = read (args !! 0) :: Int
    let height = read (args !! 1) :: Int
    t <- round . (*1) <$> getPOSIXTime
    setStdGen (mkStdGen t)
    seed  <- getStdGen
    let rs =  (randomRs (0,2) seed :: [Int])
    let maze = generate width height  rs
    print "generating maze"
    print "solving maze"
    let solution = solve maze rs
    view maze solution t
    print "done"
