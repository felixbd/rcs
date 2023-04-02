#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
This module contains the Cube class.
It is used to represent a Rubik's Cube and supports manipulation of the cube.

Copyright (C) 2021-now  Felix D.
Created by felix on 09.03.2021 (%d.%m.%Y)

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
"""

import numpy as np
from copy import deepcopy

"""
List of all the possible moves
"""
POSSIBLE_MOVES = [
    f(x)
    for x in ["F", "B", "U", "D", "L", "R"]
    for f in [lambda x: x, lambda x: x + "2", lambda x: x + "'"]
]

"""
Translate rubik's cube notation to method name and number of rotation
"""
TRANSLATE_MOVE = {
    move: (
        {"F": "front", "B": "back", "U": "up", "D": "down", "L": "left", "R": "right"}[move[0]],
        (lambda x: ["", "2", "'"].index(x[1:]) + 1)(move),
    )
    for move in POSSIBLE_MOVES
}

"""
w = white (Top face)
r = red (Left face)
b = blue (Front face)
o = orange (Right face)
g = green (Back face)
y = yellow (Bottom face)
"""
COLOR_BOARD = [[[e] * 3 for _ in range(3)] for e in "wrbogy"]

"""
The middle Letter of each face represents the face's color (will not change).

A uppercase letter is a unique identifier for a corner piece.
A lowercase letter is a unique identifier for an edge piece.
"""
LETTER_BOARD = [
    # TOP SIDE
    [["A", "a", "B"], ["d", "𝗪", "b"], ["D", "c", "C"]],
    # LEFT SIDE
    [["E", "e", "F"], ["h", "𝗥", "f"], ["H", "g", "G"]],
    # FRONT SIDE
    [["I", "i", "J"], ["l", "𝗕", "j"], ["L", "k", "K"]],
    # RIGHT SIDE
    [["M", "m", "N"], ["p", "𝗢", "n"], ["P", "o", "O"]],
    # BACK SIDE
    [["Q", "q", "R"], ["t", "𝗚", "r"], ["T", "s", "S"]],
    # BOTTOM SIDE
    [["U", "u", "V"], ["x", "𝗬", "v"], ["X", "w", "W"]],
]


class CubeObj:
    """
    Representation of a Rubik's cube with the ability to rotate the faces.
    """
    colour: bool

    def __init__(self, colour=True) -> None:
        """
        Initialize the cube
        :param colour: if True, the cube will be colored, otherwise it will be represented by old-pochman letters
        """
        self._board = COLOR_BOARD if colour else LETTER_BOARD

    def __str__(self) -> str:
        return str(np.array(self._board))

    def __eq__(self, other):
        return hash(self) == hash(other)

    def __hash__(self):
        return hash(str(self))

    def __copy__(self):
        return deepcopy(self)

    @property
    def board(self) -> list:
        return self._board

    @board.setter
    def board(self, input_board: list) -> None:
        # TODO(all) check if the given board is valid

        self._board = input_board

    def translate(self, moves: list[str]) -> None:
        """
        Translate a list of moves in method name and execute them on the cube.
        :param moves: list of moves
        :return: None
        """
        for move in moves:
            # check if the move is valid
            if move not in POSSIBLE_MOVES:
                raise ValueError(f"Move {move} is not a valid move.")

            # translate the move into a function name and call it
            method_name, count = TRANSLATE_MOVE[move]
            for _ in range(count):
                getattr(self, method_name)()

    def front(self) -> None:
        """
        Rotating the front side by 90 degrees clockwise. (blue side)
        """
        # get the current state of the cube
        white_row = self._board[0][2]
        orange_row = [item[0] for item in self._board[3]]
        yellow_row = self._board[5][0]
        red_row = [item[2] for item in self._board[1]]

        # update the cube
        self._board[0][2] = red_row[::-1]
        for index in range(3):
            self._board[3][index][0] = white_row[index]
        self._board[5][0] = orange_row[::-1]
        for index in range(3):
            self._board[1][index][2] = yellow_row[index]
        self._board[2] = np.rot90(np.array(self._board[2]), 3).tolist()

    def back(self) -> None:
        """
        Rotation of the back side by 90 degrees clockwise. (green side)
        """
        # get the current state of the cube
        white_row = self._board[0][0][::-1]
        orange_row = [item[2] for item in self._board[3]]
        yellow_row = self._board[5][2][::-1]
        red_row = [item[0] for item in self._board[1]]

        # update the cube
        self._board[0][0] = orange_row
        for index in range(3):
            self._board[3][index][2] = yellow_row[index]
        self._board[5][2] = red_row
        for index in range(3):
            self._board[1][index][0] = white_row[index]
        self._board[4] = np.rot90(np.array(self._board[4]), 1).tolist()

    def right(self) -> None:
        """
        Rotation of the right side by 90 degrees clockwise. (orange side)
        """
        # get the current state of the cube
        white_row = [item[2] for item in self._board[0]][::-1]
        green_row = [item[2] for item in self._board[4]][::-1]
        yellow_row = [item[2] for item in self._board[5]]
        blue_row = [item[2] for item in self._board[2]]

        # update the cube
        for index in range(3):
            self._board[0][index][2] = blue_row[index]
        for index in range(3):
            self._board[4][index][2] = white_row[index]
        for index in range(3):
            self._board[5][index][2] = green_row[index]
        for index in range(3):
            self._board[2][index][2] = yellow_row[index]
        self._board[3] = np.rot90(np.array(self._board[3]), 3).tolist()

    def left(self) -> None:
        """
        Rotating the left side by 90 degrees clockwise. (red side)
        """
        # get the current state of the cube
        white_row = [item[0] for item in self._board[0]]
        blue_row = [item[0] for item in self._board[2]]
        yellow_row = [item[0] for item in self._board[5]][::-1]
        green_row = [item[0] for item in self._board[4]][::-1]

        # update the cube
        for index in range(3):
            self._board[0][index][0] = green_row[index]
        for index in range(3):
            self._board[2][index][0] = white_row[index]
        for index in range(3):
            self._board[5][index][0] = blue_row[index]
        for index in range(3):
            self._board[4][index][0] = yellow_row[index]
        self._board[1] = np.rot90(np.array(self._board[1]), 3).tolist()

    def up(self) -> None:
        """
        Rotating the top side by 90 degrees clockwise. (white side)
        """
        # get the current state of the cube
        blue_row = self._board[2][0]
        red_row = self._board[1][0]
        green_row = self._board[4][0]
        orange_row = self._board[3][0]

        # update the cube
        self._board[2][0] = orange_row
        self._board[1][0] = blue_row
        self._board[4][0] = red_row[::-1]
        self._board[3][0] = green_row[::-1]
        self._board[0] = np.rot90(np.array(self._board[0]), 3).tolist()

    def down(self) -> None:
        """
        Rotating the downside by 90 degrees clockwise. (yellow side)
        """
        # get the current state of the cube
        blue_row = self._board[2][2]
        red_row = self._board[1][2]
        green_row = self._board[4][2]
        orange_row = self._board[3][2]

        # update the cube
        self._board[2][2] = red_row
        self._board[3][2] = blue_row
        self._board[4][2] = orange_row[::-1]
        self._board[1][2] = green_row[::-1]
        self._board[5] = np.rot90(np.array(self._board[5]), 3).tolist()
