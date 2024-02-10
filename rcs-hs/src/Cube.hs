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
            , translateMove) where

import Data.List (transpose, intercalate)

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
                                                         (init r) ++ [last g],
                                                         (init b) ++ [last r],
                                                         (init o) ++ [last b],
                                                         (init g) ++ [last o],
                                                         rotatingMatrixCounterClockwise y]

{-|
Rotating the left side by 90 degrees clockwise. (red side)
-}
rotateLeft :: RubiksCube -> RubiksCube
rotateLeft (RubiksCube [w, r, b, o, g, y]) = RubiksCube [nw,
                                                         rotatingMatrixClockwise r,
                                                         nb, o, ng, ny]
  where
    nb = [(head . head) w : (tail . head) b,
          head (w !! 1) : tail (b !! 1),
          head (w !! 2) : tail (b !! 2)
         ]

    ng = [(head . head) y : (tail . head) g,
          head (y !! 1) : tail (g !! 1),
          head (y !! 2) : tail (g !! 2)
         ]

    nw = [(head . head) g : (tail . head) w,
          head (g !! 1) : tail (w !! 1),
          head (g !! 2) : tail (w !! 2)
         ]

    ny = [(head . head) b : (tail . head) y,
          head (b !! 1) : tail (y !! 1),
          head (b !! 2) : tail (y !! 2)
         ]

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
