module View where


import Codec.Picture
import Data.Matrix hiding (trace)
import Debug.Trace

import Maze


data Space = P | W | S
    deriving (Show, Eq)

carve :: Maze -> Matrix Space
carve (Maze width height chart) = chisel 1 1 chart stone
    where stone = matrix (2*width+1) (2*height+1) $ \(i,j) -> if onNode (i,j) width height then P else W

onNode (i,j) w h = (mod (i) 2 == 0 && mod (j) 2 == 0 && (i) >= 2 && (j) >= 2 && (i) <= w*2 && (j) <= h*2) || (i,j) == (1,2) || (i,j) == (2*w+1,2*h)

chisel :: Int -> Int -> Matrix Node -> Matrix Space -> Matrix Space
chisel i j mnode msquare    | i == (nrows mnode) && j == (ncols mnode) = msquare
                            | i == (nrows mnode) = chisel 1 (j+1) mnode $ chiselOne node msquare
                            | otherwise = chisel (i+1) j mnode $ chiselOne node msquare
                    where   node = mnode ! (i,j)


chiselOne node msquare  | children node == [] = msquare
                        | adjnx > adjcx = chiselOne rep $ setElem P (adjnx-1,adjny) msquare
                        | adjnx < adjcx = chiselOne rep $ setElem P (adjnx+1,adjny) msquare
                        | adjny > adjcy = chiselOne rep $ setElem P (adjnx,adjny-1) msquare
                        | adjny < adjcy = chiselOne rep $ setElem P (adjnx,adjny+1) msquare
                where   adjnx = (x $ pos node) * 2
                        adjcx = (x $ pos c) * 2
                        adjny = (y $ pos node) * 2
                        adjcy = (y $ pos c) * 2
                        (c:cs) = children node
                        rep = (Node True (pos node) cs)


cut spacem sol = mapPos (\(i,j) s -> if (mod i 2 == 0) && (mod j 2 == 0) then (if sol  ! (quot i 2, quot j 2) == 1 then S else s) else s) spacem

view maze solution time display_solve =
        do
            writePng ("maze" ++ (show time) ++ ".png") $ imageCreator (carve maze)

            if display_solve == 1 then writePng ("sol" ++ (show time) ++ ".png") $ imageCreator  (cut (carve maze) solution) else return ()


imageCreator spaceMatrix = generateImage pixelRenderer ((nrows spaceMatrix)*2) ((ncols spaceMatrix)*2)
   where pixelRenderer x y  | spaceMatrix ! ((quot x 2)+1, (quot y 2)+1) == P = PixelRGB8 255 255 255
                            | spaceMatrix ! ((quot x 2)+1, (quot y 2)+1) == S = PixelRGB8 255 0 0
                            | otherwise = PixelRGB8 0 0 0

