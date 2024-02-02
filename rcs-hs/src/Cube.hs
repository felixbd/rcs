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

module Cube (RubiksCube, newCube, possibleMoves, translateMove) where

import Data.List (transpose, intercalate)

data FaceColor = White | Red | Green | Blue | Orange | Yellow deriving (Eq, Show)

newtype RubiksCube = RubiksCube { cube :: [[[Char]]] } deriving (Show)

instance Eq RubiksCube where
    (RubiksCube c1) == (RubiksCube c2) = c1 == c2

{-|
Some documentation ...
-}
-- newCube withColor = RubiksCube [replicate 3 (replicate 3 e) | e <- [White, Red, Blue, Orange,  Green, Yellow]]
newCube :: Bool -> RubiksCube
newCube withColor = RubiksCube $ if withColor
                                then [replicate 3 (replicate 3 e) | e <- "wrbogy"]
                                else [
                                    ["AaB","dWb","DcC"], ["EeF","hRf","HgG"], ["IiJ","lBj","LkK"],
                                    ["MmN", "pOn","PoO"], ["QqR","tGr","TsS"], ["UuV","xYv","XwW"]
                                ]

possibleMoves :: [String]
possibleMoves = concat [map f ["F", "B", "U", "D", "L", "R"] | f <- [id, (++"2"), (++"'")]]

translateMove :: String -> (RubiksCube -> RubiksCube)
translateMove (x:xs) = case x of
    'F' -> rep rotateFront
    'B' -> rep rotateBack
    'U' -> rep rotateUp
    'D' -> rep rotateDown
    'L' -> rep rotateLeft
    'R' -> rep rotateRight
    _   -> id
    where
        rep f = foldr (.) id (replicate (case xs of "" -> 1; "2" -> 2; "'" -> 3; _ -> 0) f)


applyMove :: [String] -> RubiksCube -> RubiksCube
applyMove xs c = foldr (.) id (map translateMove xs) c

rotatingMatrixClockwise :: [[a]] -> [[a]]
rotatingMatrixClockwise = transpose . reverse

rotatingMatrixCounterClockwise :: [[a]] -> [[a]]
rotatingMatrixCounterClockwise = reverse . transpose


{-|
Rotating the top side by 90 degrees clockwise. (white side)
-}
rotateUp :: RubiksCube -> RubiksCube
rotateUp (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube [rotatingMatrixClockwise w,
                                                         head b : tail r,
                                                         head o : tail b,
                                                         reverse (head g) : tail o,
                                                         reverse (head r) : tail g, y]

{-|
Rotating the bottom side by 90 degrees clockwise. (yellow side)
-}
rotateDown :: RubiksCube -> RubiksCube
rotateDown (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube c -- TODO

{-|
Rotating the left side by 90 degrees clockwise. (red side)
-}
rotateLeft :: RubiksCube -> RubiksCube
rotateLeft (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube c -- TODO

{-|
Rotating the right side by 90 degrees clockwise. (orange side)
-}
rotateRight :: RubiksCube -> RubiksCube
rotateRight (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube c -- TODO

{-|
Rotating the front side by 90 degrees clockwise. (blue side)
-}
rotateFront :: RubiksCube -> RubiksCube
rotateFront (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube c -- TODO

{-|
Rotating the back side by 90 degrees clockwise. (green side)
-}
rotateBack :: RubiksCube -> RubiksCube
rotateBack (RubiksCube c@[w, r, b, o, g, y]) = RubiksCube c -- TODO
