module Main where

import System.Random
import System.Environment
import Data.Time.Clock.POSIX
import System.Exit
import System.Console.ArgParser

import View
import Maze
import Stack
import Solve


data ArgSet =  -- First, we need a datatype
  ArgSet Int Int Int

parser
  :: ParserSpec ArgSet
parser = ArgSet
  `parsedBy` reqPos "width"
  `andBy` reqPos "height"
  `andBy` optPos 0 "solve"

main = withParseResult parser exec

exec (ArgSet width height sol) = do
    t <- round . (*1) <$> getPOSIXTime
    setStdGen (mkStdGen t)
    seed  <- getStdGen
    let rs =  (randomRs (0,2) seed :: [Int])
    let maze = generate width height rs
    let result = solve maze rs
    view maze result t sol
    print "done"

