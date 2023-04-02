{-|
This module contains the Cube class.
It is used to represent a Rubik's Cube and supports manipulation of the cube.

Copyright (C) 2023-now  Felix D.
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

newtype RubiksCube = RubiksCube { cube :: [[String]] } deriving (Show)

instance Eq RubiksCube where
    (RubiksCube c1) == (RubiksCube c2) = c1 == c2


{-|
Some documentation ...
-}
newCube :: Bool -> RubiksCube
newCube withColor = RubiksCube if withColor
                                then [replicate 3 (replicate 3 e) | e <- "wrbogy"]
                                else [
                                ["AaB","dWb","DcC"], ["EeF","hRf","HgG"], ["IiJ","lBj","LkK"],
                                ["MmN","pOn","PoO"], ["QqR","tGr","TsS"], ["UuV","xYv","XwW"]
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
    where
        rep f = foldr (.) id (replicate (case xs of "" -> 1; "2" -> 2; "'" -> 3; _ -> 0) f)

rotateUp :: RubiksCube -> RubiksCube
rotateUp = id

rotateDown :: RubiksCube -> RubiksCube
rotateDown = id

rotateLeft :: RubiksCube -> RubiksCube
rotateLeft = id

rotateRight :: RubiksCube -> RubiksCube
rotateRight = id

rotateFront :: RubiksCube -> RubiksCube
rotateFront = id

rotateBack :: RubiksCube -> RubiksCube
rotateBack = id
