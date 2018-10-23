module Solve where


import Data.Matrix
import Maze
import Stack




solve :: Maze -> [Int] -> Matrix Int
solve maze rs = solve_ unvisited mx start rs empty 0
        where   start = mlookup maze (Pos 1 1)
                unvisited = (Maze (width maze) (height maze) newChart)
                newChart =  mapPos (\(i,j) n -> (Node False (Pos i j) (children n))) (chart maze)
                mx = matrix (width maze) (height maze) (\p -> 0)




solve_ maze matrix node (r:rs) stack run   | isEmpty stack && run /= 0 = matrix
                                           |  (xcurr, ycurr) == end = setElem 1 end matrix
                                           | length unvisitedChildren == 0 = solve_ newMaze (setElem 0 (xcurr, ycurr) matrix) (peek stack) rs (pop stack) (run+1)
                                           | otherwise = solve_ newMaze (setElem 1 (xcurr, ycurr) matrix) nextNode rs (push stack node) (run+1)
                        where   (xcurr, ycurr) = (x $ pos node, y $ pos node)
                                end = (width maze, height maze)
                                unvisitedChildren = filter (not . visited) (map (\n -> mlookup maze (pos n)) (children (mlookup maze (Pos xcurr ycurr))))
                                nextNode = unvisitedChildren !! (mod r (length unvisitedChildren))
                                newMaze = (Maze (width maze) (height maze) newChart)
                                newChart = setElem (Node True (Pos xcurr ycurr) (children node)) (xcurr, ycurr) (chart maze)
