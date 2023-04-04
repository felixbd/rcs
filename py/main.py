#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
The following code is a part of the Rubik's Cube Solver project.
It uses the cube.py file to create a cube object and solve it using the old pochmann method.

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

import timeit
# import time
import random as rm
from string import ascii_lowercase

from cube import CubeObj as Cube, POSSIBLE_MOVES

"""
Dictionary for mapping a cube coordinate to old pochmann letter code
"""
INDEX_TO_NAME_CENTER = {
    (x, y, z): l
    for ((x, y, z), l) in zip(
        ((i, j, k) for i in range(6) for j, k in [(0, 1), (1, 2), (2, 1), (1, 0)]),
        ascii_lowercase[:24],
    )
}

"""
Translate color code of center pieces to unique (old pochmann) letter
"""
NAME_OF_BUFFER_PIECES_CENTER = {
    ("w", "g"): "a", ("w", "o"): "b", ("w", "b"): "c", ("w", "r"): "d",  # top face
    ("r", "w"): "e", ("r", "b"): "f", ("r", "y"): "g", ("r", "g"): "h",  # left face
    ("b", "w"): "i", ("b", "o"): "j", ("b", "y"): "k", ("b", "r"): "l",  # front face
    ("o", "w"): "m", ("o", "g"): "n", ("o", "y"): "o", ("o", "b"): "p",  # right face
    ("g", "w"): "q", ("g", "o"): "r", ("g", "y"): "s", ("g", "r"): "t",  # back face
    ("y", "b"): "u", ("y", "o"): "v", ("y", "g"): "w", ("y", "r"): "x",  # bottom face
}

"""
Translate color code of corner pieces to unique (old pochmann) letter
You should read the color of the corner piece from the top, left side and back
"""
NAME_OF_BUFFER_PIECES_CORNER = {
    ("w", "r", "g"): "A", ("r", "g", "w"): "E", ("g", "w", "r"): "Q",  # TOP LEFT BACK
    ("w", "g", "o"): "B", ("o", "w", "g"): "N", ("g", "o", "w"): "R",  # TOP RIGHT BACK
    ("w", "o", "b"): "C", ("b", "w", "o"): "J", ("o", "b", "w"): "M",  # TOP RIGHT FRONT
    ("w", "b", "r"): "D", ("r", "w", "b"): "F", ("b", "r", "w"): "I",  # TOP LEFT FRONT
    ("r", "y", "g"): "H", ("g", "r", "y"): "T", ("y", "g", "r"): "X",  # BOTTOM LEFT BACK
    ("o", "g", "y"): "O", ("g", "y", "o"): "S", ("y", "o", "g"): "W",  # BOTTOM RIGHT BACK
    ("b", "o", "y"): "K", ("o", "y", "b"): "P", ("y", "b", "o"): "V",  # BOTTOM RIGHT FRONT
    ("r", "b", "y"): "G", ("b", "y", "r"): "L", ("y", "r", "b"): "U",  # BOTTOM LEFT FRONT
}

"""
PERMS USED:
I.   | Y-Perm   / Ecken-Algorithmus     |  => (F) R U' R' U' R U R' F' R U R' U' R' F R (F')
II.  | R-Perm   / Parity                |  => R U' R' U' R U R D R' U' R D' R' U2 R' U'
III. | T-Perm   / Kanten-Algorithmus 1  |  => R U R' U' R' F R2 U' R' U' R U R' F'
IV.  | J-Perm a / Kanten-Algorithmus 2  |  => R U R' F' R U R' U' R' F R2 U' R' U'
V.   | J-Perm b / Kanten-Algorithmus 3  |  => U' R' U L' U2 R U' R' U2 L R
"""
Y_PERM = "F R U' R' U' R U R' F' R U R' U' R' F R F'"
R_PERM = "R U' R' U' R U R D R' U' R D' R' U2 R' U'"  # Parity Fixing algorithm
T_PERM = "R U R' U' R' F R2 U' R' U' R U R' F'"
J_PERM_DOWN = "R U R' F' R U R' U' R' F R2 U' R' U'"
J_PERM_UP = "U' R' U L' U2 R U' R' U2 L R"

"""
Permutation used to swap the current buffer piece with the correct one
"""
MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CENTER = {
    "a": f"{J_PERM_UP}",
    # 'b': f"",  # buffer piece is already in the correct position but not oriented
    "c": f"{J_PERM_DOWN}",
    "d": f"{T_PERM}",
    "e": f"R L F R' {J_PERM_DOWN} R F' L' R'",
    "f": f"R F R' {J_PERM_DOWN} R F' R'",
    "g": f"L' R F R' {J_PERM_DOWN} R F' R' L",
    "h": f"U B' U' {T_PERM} U B U'",
    "i": f"R2 U' R' F' R' {J_PERM_DOWN} R F R U R2",
    "j": f"U2 R U2 {T_PERM} U2 R' U2",
    "k": f"R F R' L' {T_PERM} L R F' R'",
    "l": f"L' {T_PERM} L",
    # 'm': f"",  # buffer piece is already in the correct position but not oriented
    "n": f"U B U' {T_PERM} U B' U'",
    "o": f"D' R F R' L' {T_PERM} L R F' R' D",
    "p": f"U' F' U {T_PERM} U' F U",
    "q": f"R2 U R' F' R' {J_PERM_DOWN} R F R U' R2",
    "r": f"U2 R' U2 {T_PERM} U2 R U2",
    "s": f"D L R' B' R {J_PERM_UP} R' B L' R D'",
    "t": f"L {T_PERM} L'",
    "u": f"R F2 R' {J_PERM_DOWN} R F2 R'",
    "v": f"D' R F2 R' {J_PERM_DOWN} R F2 R' D",
    "w": f"R' B2 R {J_PERM_UP} R' B2 R",
    "x": f"D R F2 R' {J_PERM_DOWN} R F2 R' D'",
}

"""
Translate current buffer piece letter to solving algorithm
"""
MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CORNER = {
    # 'A': f"",  # buffer piece is already in the correct position but not oriented
    "B": f"U {J_PERM_UP} U'",
    "C": f"{Y_PERM}",
    "D": f"U2 {J_PERM_DOWN} U2",
    # 'E': f"",  # buffer piece is already in the correct position but not oriented
    "F": f"F {Y_PERM} F'",
    "G": f"D R {Y_PERM} R' D'",
    "H": f"D2 F' {Y_PERM} F D2",
    "I": f"F R U {J_PERM_UP} U' R' F'",
    "J": f"R U {J_PERM_UP} U' R'",
    "K": f"R {Y_PERM} R'",
    "L": f"D F' {Y_PERM} F D'",
    "M": f"F' U2 {J_PERM_DOWN} U2 F",
    "N": f"R' F' U2 {J_PERM_DOWN} U2 F R",
    "O": f"D' R {Y_PERM} R' D",
    "P": f"F' {Y_PERM} F",
    # 'Q': f"",  # buffer piece is already in the correct position but not oriented
    "R": f"R' {Y_PERM} R",
    "S": f"D' F' {Y_PERM} F D",
    "T": f"D2 R {Y_PERM} R' D2",
    "U": f"F2 {Y_PERM} F2",
    "V": f"D' F2 {Y_PERM} F2 D",
    "W": f"R2 {Y_PERM} R2",
    "X": f"D' F2 {Y_PERM} F2 D",
}


def normalize_instructions(instruction: str) -> list[str]:
    """
    Formats the Rubik's Cube instruction so that it can be understood by the qube class
    :param instruction: the instruction to be formatted
    :return: a list of instructions
    """
    return (
        instruction.upper()
        .translate(str.maketrans({"’": "'", "`": "'", '"': "'", "(": "", ")": ""}))
        .split()
    )


def timer(func):
    def wrapper(*args, **kwargs):
        # start_time = time.time()
        # result = func(*args, **kwargs)
        # elapsed_time = time.time() - start_time
        # print(f"Elapsed time (time) for function '{func.__name__}': {elapsed_time:.6f} seconds")

        # start_time = time.perf_counter()
        # result = func(*args, **kwargs)
        # elapsed_time = time.perf_counter() - start_time
        # print(f"Elapsed time (perf_counter) for function '{func.__name__}': {elapsed_time:.6f} seconds")

        start_time = timeit.default_timer()
        result = func(*args, **kwargs)
        elapsed_time = timeit.default_timer() - start_time
        print(
            f"Elapsed time (timeit) for function '{func.__name__}': {elapsed_time:.6f} seconds"
        )

        return result
    return wrapper


def create_random_scrambled_cube() -> tuple[Cube, list[str]]:
    """
    Create a random Rubik’s Cube
    :return: the cube as an object
    """
    my_cube = Cube(color=True)
    random_moves = rm.choices(POSSIBLE_MOVES, k=rm.randint(10, 15))
    my_cube.translate(random_moves)

    return my_cube, random_moves


def create_scrambled_cube(scramble: list[str]) -> Cube:
    """
    Create a Rubik’s Cube with a given scramble
    :param scramble: the scramble to be applied to the cube
    :return: the cube as an object
    """
    for move in scramble:
        if move not in POSSIBLE_MOVES:
            raise ValueError(f"->{move}<- is not a valid move.")

    # if not all(map(lambda x: x in POSSIBLE_MOVES, scramble)):
    #    raise ValueError('INVALID INPUT')

    my_cube = Cube(color=True)
    my_cube.translate(scramble)

    return my_cube


@timer
def solve_old_pochmann(cube: Cube) -> list[str]:
    """
    Solve the cube using the old pochmann method
    :param cube: the cube to be solved
    :return: the solution to the cube
    """
    # list of moves to solve the cube
    moves: list[str] = []

    def find_not_solved_center() -> str | None:
        """
        :return: name of first not solved centerpiece, if there is none return None
        """
        for index in range(6):
            color_of_face = cube.board[index][1][1]

            # check if each centerpiece of the current face is at its target location

            if cube.board[index][0][1] != color_of_face:
                # search for the first not solved centerpieces that is also not the correct pieces for the buffer
                if INDEX_TO_NAME_CENTER[(index, 0, 1)] != "m":
                    return INDEX_TO_NAME_CENTER[(index, 0, 1)]

            if cube.board[index][1][0] != color_of_face:
                return INDEX_TO_NAME_CENTER[(index, 1, 0)]

            if cube.board[index][1][2] != color_of_face:
                # search for the first not solved centerpieces that is also not the correct pieces for the buffer
                if INDEX_TO_NAME_CENTER[(index, 1, 2)] != "b":
                    return INDEX_TO_NAME_CENTER[(index, 1, 2)]

            if cube.board[index][2][1] != color_of_face:
                return INDEX_TO_NAME_CENTER[(index, 2, 1)]

        return None

    def move_center_buffer_to_target_location() -> None:
        """
        Moves the centerpieces in the buffer to its target location
        :return: None
        """
        buffer_letter = NAME_OF_BUFFER_PIECES_CENTER[
            (cube.board[0][1][2], cube.board[3][0][1])
        ]

        # if the current buffer pieces is also the correct pieces for the buffer
        #  ignore the current buffer pieces and search for another not solved centerpieces
        if buffer_letter in "bm":
            new_buffer = find_not_solved_center()
            if new_buffer is not None:
                # there is still a not solved centerpieces
                buffer_letter = new_buffer
            else:
                # all centerpieces are solved
                return

        moves.append(buffer_letter)
        move = normalize_instructions(
            MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CENTER[buffer_letter]
        )
        cube.translate(move)

        # enter recursion as long as there are still not solved centerpieces
        move_center_buffer_to_target_location()

    def find_not_solved_corners() -> list[str]:
        """
        :return: list of names of unsolved corner pieces, if there is none return the empty list
        """

        """
        TODO: the order of the elements in the second list of the zip are not quite right
        
        return ["BCDFGHIJKLMNOPRSTUVWX"[i]
            for (i, (a, b, c))
            in zip(range(21),
                   [(i, j, k)
                    for i in range(6)
                    for j, k in [(0, 0), (0, 2), (2, 0), (2, 2)]
                    if i not in [0, 1, 4] or j != 0 or k != 0
                    ])
            if cube.board[a][b][c] != 'wrbogy'[a]
            ]
        """

        return ["BCDFGHIJKLMNOPRSTUVWX"[i]
                for (i, (a, b, c))
                in zip(range(21),
                       [(0, 0, 2), (0, 2, 2), (0, 2, 0),             # TOP Face
                        (1, 0, 2), (1, 2, 2), (1, 2, 0),             # LEFT Face
                        (2, 0, 0), (2, 0, 2), (2, 2, 0), (2, 2, 2),  # FRONT Face
                        (3, 0, 0), (3, 0, 2), (3, 2, 0), (3, 2, 2),  # RIGHT Face
                        (4, 0, 2), (4, 2, 2), (4, 2, 0),             # BACK Face
                        (5, 0, 0), (5, 0, 2), (5, 2, 2), (5, 2, 0)   # BOTTOM Face
                        ])
                if cube.board[a][b][c] != 'wrbogy'[a]
                ]

    def move_corner_buffer_to_target_location() -> None:
        """
        Moves the corner pieces in the buffer to its target location
        :return: None
        """

        buffer = NAME_OF_BUFFER_PIECES_CORNER[
            (cube.board[0][0][0], cube.board[1][0][0], cube.board[4][0][0])
        ]

        # DO NOT REMOVE THIS
        #  if you remove this, the algorithm will not terminate in approximately 15% of the cases
        #  I have no idea why this is the case, should work without, but apparently it doesn't
        if len(moves) > 10:  # the threshold is totally arbitrary
            if set(moves[-4:]) == {'X', 'V'}:  # if the last 4 moves are X and V we entert a bad recursion
                new_buffer = find_not_solved_corners()
                if len(new_buffer) > 0:
                    rm.shuffle(new_buffer)
                    buffer = new_buffer[0]

        # if the current buffer pieces is also the correct pieces for the buffer
        #  ignore the current buffer pieces and search for another not solved corner pieces
        if buffer in "AEQ":
            new_buffer = find_not_solved_corners()
            if len(new_buffer) > 0:
                # there is still a not solved corner pieces
                rm.shuffle(new_buffer)
                buffer = new_buffer[0]
            else:
                # all corner pieces are solved
                return

        moves.append(buffer)
        move = normalize_instructions(
            MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CORNER[buffer]
        )
        cube.translate(move)

        # enter recursion as long as there are still not solved corner pieces
        move_corner_buffer_to_target_location()

    # solve all centerpieces
    move_center_buffer_to_target_location()

    # if the number of moves is not even, the parity algorithm has to be applied
    if len(moves) > 0 and len(moves) % 2 != 0:
        cube.translate(normalize_instructions(R_PERM))
        moves.append("Parity")

    # solve all corner pieces
    move_corner_buffer_to_target_location()

    return moves


def solution_schedule() -> None:
    user_input = input('Do you want to create a cube with a RANDOM SCRAMBLE or use your OWN SCRAMBLE? [R/o]').lower()

    if user_input not in "ro":
        raise ValueError("INVALID INPUT")

    if user_input == "o":
        scramble: list[str] = normalize_instructions(input("Enter your scramble: "))

        if not all(map(lambda x: x in POSSIBLE_MOVES, scramble)):
            raise ValueError("INVALID INPUT")

        test_cube = create_scrambled_cube(scramble)
    else:
        test_cube, scramble = create_random_scrambled_cube()

    # ---------------------------- solve the cube ----------------------------

    print(f"Scramble: {' '.join(scramble)}\n"
          f"Scrambled Cube:\n\n{str(test_cube)}\n")

    solution = solve_old_pochmann(test_cube)
    edge_count = sum(map(lambda x: x in ascii_lowercase, solution))
    s_center, s_corner = solution[:edge_count], solution[edge_count:]

    solution_moves = []
    solution_moves += map(lambda x: MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CENTER[x], s_center)

    if s_corner[0] == "Parity":
        solution_moves.append(R_PERM)
        s_corner = s_corner[1:]

    solution_moves += map(lambda x: MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CORNER[x], s_corner)

    print(f"Solution: {' '.join(solution)}\n"
          f"Permutation: {' '.join(solution_moves)}\n"
          f"Solution score: {len((' '.join(solution_moves)).split()) / 20} (1.0 is optimal)\n"
          f"Solved Cube:\n\n{str(test_cube)}")


def test() -> None:
    def create_random_cube_and_solve() -> None:
        cube, scramble = create_random_scrambled_cube()
        print(f"{'-'*80}\nscramble: {' '.join(scramble)}")
        solving_moves = solve_old_pochmann(cube)
        print(f"Solving moves: {' '.join(solving_moves)}")

    number = 100
    total_time = timeit.timeit(create_random_cube_and_solve, number=number)
    print(f"{'='*80}\n"
          f"Total time for {number} cubes: {total_time} seconds\n"
          f"Average time: {total_time / number} seconds\n"
          f"{'='*80}")


if __name__ == "__main__":
    solution_schedule()
    # test()
