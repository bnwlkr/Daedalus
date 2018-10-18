module Maze where

import Data.Matrix

data Maze = Maze {
        width :: Int,
        height :: Int,
        chart :: Matrix Node
    }
    deriving (Show)

data Node = Node {
        visited :: Bool,
        position :: Position,
        children :: [Node]
    }


instance Show Node where
    show n  | visited n = "v"
            | otherwise = "_"

data Position = Pos {
        x :: Int,
        y :: Int
    }
    deriving (Show, Eq)

instance Ord Position where
     p1 <= p2 = (x p1) <= (x p2) && (y p1) <= (y p2)

--generate :: Maze -> Position -> [Int] -> (Stack Node) -> Maze


adjacent :: Maze -> Node -> [Node]
adjacent maze node = map (mlookup maze) $ filter (inMaze maze) [(Pos (i+1) j),(Pos (i-1) j),(Pos i (j+1)),(Pos i (j-1))]
            where   i = x (position node)
                    j = y (position node)

firstInRange :: [Int] -> Int -> Int
firstInRange (x:xs) s   | x < s = x
                        | otherwise = firstInRange xs s


mlookup :: Maze -> Position -> Node
mlookup maze pos = chart maze ! (x pos, y pos)

inMaze :: Maze -> Position -> Bool
inMaze maze pos = pos >= (Pos 1 1) && pos <= (Pos (width maze) (height maze))



-----------------

blank w h = Maze w h $ matrix w h $ \(i,j) -> Node False (Pos i j) []








