{-|
This module contains the Cube class.
It is used to represent a Rubik's Cube and supports manipulation of the cube.

Copyright Felix Drees (C) 2024 
Created by felix on 30.03.2023 (%d.%m.%Y)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

module Cube (RubiksCube
            , newCube
            , possibleMoves
            , translateMove
            , applyMove
            ) where

import Data.List (transpose)  -- , intercalate)

data FaceColor = White | Red | Green | Blue | Orange | Yellow deriving (Eq, Show)

newtype RubiksCube = RubiksCube { getBoard :: [[[FaceColor]]] } deriving (Show)

instance Eq RubiksCube where
    c1 == c2 = getBoard c1 == getBoard c2

colorMap :: Char -> FaceColor
colorMap c = case c of
        'w' -> White
        'r' -> Red
        'b' -> Blue
        'o' -> Orange
        'g' -> Green
        'y' -> Yellow
        -- otherwise -> error $ "[COLOR MAP ERROR] invalid char ->" ++ c : "<-"
        _ -> error $ "[COLOR MAP ERROR] invalid char ->" ++ c : "<-"

{-|
Some documentation ...
-}
newCube :: RubiksCube
newCube = RubiksCube [replicate 3 (replicate 3  (colorMap e)) | e <- "wrbogy"]

possibleMoves :: [String]
possibleMoves = concat [map f ["F", "B", "U", "D", "L", "R"] | f <- [id, (++"2"), (++"'")]]

-- begin moves -----------------------------------------------------------------

rotatingMatrixClockwise :: [[a]] -> [[a]]
rotatingMatrixClockwise = transpose . reverse

rotatingMatrixCounterClockwise :: [[a]] -> [[a]]
rotatingMatrixCounterClockwise = reverse . transpose

{-|
Rotating the top side by 90 degrees clockwise. (white side)
-}
rotateUp :: RubiksCube -> RubiksCube
rotateUp (RubiksCube [w, r, b, o, g, y]) = RubiksCube [rotatingMatrixClockwise w,
                                                       head b : tail r,
                                                       head o : tail b,
                                                       reverse (head g) : tail o,
                                                       reverse (head r) : tail g,
                                                       y]

{-|
Rotating the bottom side by 90 degrees clockwise. (yellow side)
-}
rotateDown :: RubiksCube -> RubiksCube
rotateDown (RubiksCube [w, r, b, o, g, y]) = RubiksCube [w,
                                                         (init r) ++ [(reverse . last) g],
                                                         (init b) ++ [last r],
                                                         (init o) ++ [last b],
                                                         (init g) ++ [(reverse . last) o],
                                                         rotatingMatrixCounterClockwise y]

{-|
Rotating the left side by 90 degrees clockwise. (red side)
-}
rotateLeft :: RubiksCube -> RubiksCube
rotateLeft (RubiksCube [w, r, b, o, g, y]) = RubiksCube [nw,
                                                         rotatingMatrixClockwise r,
                                                         nb, o, ng, ny]
  where
    nb = [head (w !! i) : tail (b !! i) | i <- [0..2]]
    ny = [head (b !! i) : tail (y !! i) | i <- [0..2]]
    ng = [head (y !! i) : tail (g !! j) | (i, j) <- [(2, 0), (1, 1), (0, 2)]]
    nw = [head (g !! i) : tail (w !! j) | (i, j) <- [(2, 0), (1, 1), (0, 2)]]

{-|
Rotating the right side by 90 degrees clockwise. (orange side)
-}
rotateRight :: RubiksCube -> RubiksCube
rotateRight (RubiksCube [w, r, b, o, g, y]) = RubiksCube [nw, r, nb,
                                                          rotatingMatrixClockwise o,
                                                          ng, ny]
  where
    nw = [init (w !! i) ++ [last (b !! i)] | i <- [0..2]]
    nb = [init (b !! i) ++ [last (y !! i)] | i <- [0..2]]
    ng = [init (g !! i) ++ [last (w !! j)] | (i, j) <- [(0, 2), (1, 1), (2, 0)]]
    ny = [init (y !! i) ++ [last (g !! j)] | (i, j) <- [(0,2), (1, 1), (2, 0)]]

{-|
Rotating the front side by 90 degrees clockwise. (blue side)
-}
rotateFront :: RubiksCube -> RubiksCube
rotateFront (RubiksCube [w, r, b, o, g, y]) = RubiksCube [nw, nr,
                                                          rotatingMatrixClockwise b,
                                                          no, g, ny]
  where
    nw = init w ++ [ [last (r !! i) | i <- [2,1,0]] ]
    nr = [init (r !! i) ++ [ last (y !! i) ] | i <- [0..2]]
    no = [(last w) !! i : tail (o !! i) | i <- [0..2]]
    ny = [[head (o !! i) | i <- [2,1,0]]] ++ tail y

{-|
Rotating the back side by 90 degrees clockwise. (green side)
-}
rotateBack :: RubiksCube -> RubiksCube
rotateBack (RubiksCube [w, r, b, o, g, y]) = RubiksCube [nw, nr, b, no,
                                                           rotatingMatrixCounterClockwise g,
                                                           ny]
  where
    nw = [ [last (o !! i) | i <- [0..2]] ] ++ tail w
    nr = [ [(head w) !! i] ++ tail (r !! j) | (i, j) <- [(2, 0), (1, 1), (0, 2)]]
    no = [ init (o !! i) ++ [(last y) !! j] | (i, j) <- [(0, 2), (1, 1), (2, 0)] ]
    ny = init y ++ [[ head (r !! i) | i <- [0..2] ]]

-- end moves -------------------------------------------------------------------

translateMove :: String -> (RubiksCube -> RubiksCube)
translateMove [] = id
translateMove xss@(x:xs) = if not (elem xss possibleMoves)
                           then error $ "[TRANSLATE MOVE ERROR] invalid move ->" ++ xss ++ "<-"
                           else case x of
    'F' -> rep rotateFront
    'B' -> rep rotateBack
    'U' -> rep rotateUp
    'D' -> rep rotateDown
    'L' -> rep rotateLeft
    'R' -> rep rotateRight
    _   -> error $ "[TRANSLATE MOVE ERROR] unknown move ->" ++ x : "<-"
    where
        rep f = foldr (.) id (replicate (case xs of "" -> 1; "2" -> 2; "'" -> 3; _ -> 0) f)


applyMove :: [String] -> RubiksCube -> RubiksCube
applyMove xs c = foldr (.) id (map translateMove xs) c

---
