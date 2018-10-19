module Maze where

import Data.Matrix hiding (trace)
import Data.Maybe
import Debug.Trace

import Stack

data Maze = Maze {
        width :: Int,
        height :: Int,
        chart :: Matrix Node
    }

instance Show Maze where
    show m = show (chart m)

data Node = Node {
        visited :: Bool,
        pos :: Position,
        children :: [Node]
    }
    deriving (Eq)

instance Show Node where
    show n  | visited n = "+"
            | otherwise = "_"


data Position = Pos {
        x :: Int,
        y :: Int
    }
    deriving (Show, Eq)

instance Ord Position where
     p1 <= p2 = (x p1) <= (x p2) && (y p1) <= (y p2)
     p1 >= p2 = (x p1) >= (x p2) && (y p1) >= (y p2)

adjacent :: Maze -> Node -> [Node]
adjacent maze node =  map (mlookup maze) $ filter (inMaze maze) [(Pos (i+1) j),(Pos (i-1) j),(Pos i (j+1)),(Pos i (j-1))]
            where   i = x (pos node)
                    j = y (pos node)

mlookup :: Maze -> Position -> Node
mlookup maze pos =  chart maze ! (x pos, y pos)

inMaze :: Maze -> Position -> Bool
inMaze maze pos = pos >= (Pos 1 1) && pos <= (Pos (width maze) (height maze))

generate_ maze node (r:rs) stack run    |  isEmpty stack && run /= 0 = maze
                                    |  next == Nothing =  generate_ newMaze (peek stack) rs (pop stack) (run+1)
                                    |  otherwise = generate_ newMaze (fromJust next) rs (push stack nodeNewChild) (run+1)

                where next   | length adjacents == 0 = Nothing
                             | otherwise = (trace (show (head (filter (\x -> x < (length adjacents)) (r:rs))))) $ Just (adjacents !! 0)
                      adjacents = (trace (show (chart maze))) $ filter (not . visited) (adjacent maze node)
                      nodeNewChild = (Node True (pos node) $ (fromJust next):children node)
                      newMaze = (Maze (width maze) (height maze) (setElem (Node True (pos node) (children node)) (x $ pos node, y $ pos node) (chart maze)))

generate w h rs = generate_ (blank w h) (Node True (Pos 1 1) []) rs empty 0


-----------------

blank w h = Maze w h $ matrix w h $ \(i,j) -> Node False (Pos i j) []
