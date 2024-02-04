// Copyright (C) 2022-now by Felix D | GNU GPLv2
// Created by felix on 12.10.2022 (dd.mm.yyyy)
//

// prevent double inclusion
#pragma once

#ifndef RUBIKS_CUBE_HH_
#define RUBIKS_CUBE_HH_

// #include <iostream>
#include <sstream>
// #include <vector>
#include <array>
// #include <iostream>
#include <map>
#include <random>
#include <string>
// #include <vector>
#include <tuple>
#include <iostream>
#include <vector>

namespace rubikscube {

using ROW = std::array<int, 3>;
using FACE = std::array<ROW, 3>;
using BOARD = std::array<FACE, 6>;

/**
 * Returns a string with all Values of the given Face of the Cube.
 * */
std::string printFace(FACE face);

/**
 * Rotate 3x3 Matrix n-times by 90⁰ Clockwise.
 * */
FACE rotateMatrix(FACE a, int num);

/**
 * SOLVED_BOARD contains a list of all faces of a solved cube.
 *
 * https://stackoverflow.com/a/62252088
 * >>Note the extra set of braces. It seems a bit odd but ...<<
 * */
const BOARD SOLVED_BOARD = {{{{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}},
                             {{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}},
                             {{{2, 2, 2}, {2, 2, 2}, {2, 2, 2}}},
                             {{{3, 3, 3}, {3, 3, 3}, {3, 3, 3}}},
                             {{{4, 4, 4}, {4, 4, 4}, {4, 4, 4}}},
                             {{{5, 5, 5}, {5, 5, 5}, {5, 5, 5}}}}};

/**
 * Dictionary for mapping a cube coordinate to old pochmann letter code
 */
const std::map<std::tuple<int, int, int>, char> INDEX_2_NAME_CENTER = {
    {{0, 0, 1}, 'a'}, {{0, 1, 2}, 'b'}, {{0, 2, 1}, 'c'}, {{0, 1, 0}, 'd'},
    {{1, 0, 1}, 'e'}, {{1, 1, 2}, 'f'}, {{1, 2, 1}, 'g'}, {{1, 1, 0}, 'h'},
    {{2, 0, 1}, 'i'}, {{2, 1, 2}, 'j'}, {{2, 2, 1}, 'k'}, {{2, 1, 0}, 'l'},
    {{3, 0, 1}, 'm'}, {{3, 1, 2}, 'n'}, {{3, 2, 1}, 'o'}, {{3, 1, 0}, 'p'},
    {{4, 0, 1}, 'q'}, {{4, 1, 2}, 'r'}, {{4, 2, 1}, 's'}, {{4, 1, 0}, 't'},
    {{5, 0, 1}, 'u'}, {{5, 1, 2}, 'v'}, {{5, 2, 1}, 'w'}, {{5, 1, 0}, 'x'}};

/**
 * Translate color code of center pieces to unique (old pochmann) letter
 * */
const std::map<std::tuple<char, char>, char> NAME_OF_BUFFER_PIECES_CENTER = {
    {{'w', 'g'}, 'a'}, {{'w', 'o'}, 'b'}, {{'w', 'b'}, 'c'}, {{'w', 'r'}, 'd'},
    {{'r', 'w'}, 'e'}, {{'r', 'b'}, 'f'}, {{'r', 'y'}, 'g'}, {{'r', 'g'}, 'h'},
    {{'b', 'w'}, 'i'}, {{'b', 'o'}, 'j'}, {{'b', 'y'}, 'k'}, {{'b', 'r'}, 'l'},
    {{'o', 'w'}, 'm'}, {{'o', 'g'}, 'n'}, {{'o', 'y'}, 'o'}, {{'o', 'b'}, 'p'},
    {{'g', 'w'}, 'q'}, {{'g', 'o'}, 'r'}, {{'g', 'y'}, 's'}, {{'g', 'r'}, 't'},
    {{'y', 'b'}, 'u'}, {{'y', 'o'}, 'v'}, {{'y', 'g'}, 'w'}, {{'y', 'r'}, 'x'}};

/**
 * Translate color code of corner pieces to unique (old pochmann) letter
 * You should read the color of the corner piece from the top, left to back side
 * */
const std::map<std::tuple<char, char, char>, char> NAME_OF_BUFFER_PIECES_CORNER = {
    {{'w', 'r', 'g'}, 'A'}, {{'r', 'g', 'w'}, 'E'}, {{'g', 'w', 'r'}, 'Q'},
    {{'w', 'g', 'o'}, 'B'}, {{'o', 'w', 'g'}, 'N'}, {{'g', 'o', 'w'}, 'R'},
    {{'w', 'o', 'b'}, 'C'}, {{'b', 'w', 'o'}, 'J'}, {{'o', 'b', 'w'}, 'M'},
    {{'w', 'b', 'r'}, 'D'}, {{'r', 'w', 'b'}, 'F'}, {{'b', 'r', 'w'}, 'I'},
    {{'r', 'y', 'g'}, 'H'}, {{'g', 'r', 'y'}, 'T'}, {{'y', 'g', 'r'}, 'X'},
    {{'o', 'g', 'y'}, 'O'}, {{'g', 'y', 'o'}, 'S'}, {{'y', 'o', 'g'}, 'W'},
    {{'b', 'o', 'y'}, 'K'}, {{'o', 'y', 'b'}, 'P'}, {{'y', 'b', 'o'}, 'V'},
    {{'r', 'b', 'y'}, 'G'}, {{'b', 'y', 'r'}, 'L'}, {{'y', 'r', 'b'}, 'U'}};

/**
 * PERMS USED:
 * I.   | Y-Perm   / Ecken-Algorithmus     |  => (F) R U' R' U' R U R' F' R U R' U' R' F R (F')
 * II.  | R-Perm   / Parity                |  => R U' R' U' R U R D R' U' R D' R' U2 R' U'
 * III. | T-Perm   / Kanten-Algorithmus 1  |  => R U R' U' R' F R2 U' R' U' R U R' F'
 * IV.  | J-Perm a / Kanten-Algorithmus 2  |  => R U R' F' R U R' U' R' F R2 U' R' U'
 * V.   | J-Perm b / Kanten-Algorithmus 3  |  => U' R' U L' U2 R U' R' U2 L R
 */
/*
const std::vector<std::string> Y_PERM = {"F", "R", "U'", "R'", "U'", "R", "U", "R'", "F'", "R", "U", "R'", "U'", "R'", "F", "R", "F'"};
const std::vector<std::string> R_PERM = {"R", "U'", "R'", "U'", "R", "U", "R", "D", "R'", "U'", "R", "D'", "R'", "U2", "R'", "U'"};  // Parity Fixing algorithm
const std::vector<std::string> T_PERM = {"R", "U", "R'", "U'", "R'", "F", "R2", "U'", "R'", "U'", "R", "U", "R'", "F'"};
const std::vector<std::string> J_PERM_DOWN = {"R", "U", "R'", "F'", "R", "U", "R'", "U'", "R'", "F", "R2", "U'", "R'", "U'"};
const std::vector<std::string> J_PERM_UP = {"U'", "R'", "U", "L'", "U2", "R", "U'", "R'", "U2", "L", "R"};
*/

// TODO maybe use `const char ABC[] = "...";`
const std::string Y_PERM = "F R U' R' U' R U R' F' R U R' U' R' F R F'";
const std::string R_PERM = "R U' R' U' R U R D R' U' R D' R' U2 R' U'";
const std::string T_PERM = "R U R' U' R' F R2 U' R' U' R U R' F'";
const std::string J_PERM_DOWN = "R U R' F' R U R' U' R' F R2 U' R' U'";
const std::string J_PERM_UP = "U' R' U L' U2 R U' R' U2 L R";


/**
 * Permutation used to swap the current buffer piece with the correct one
 * */
const std::map<char, std::string> MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CENTER = {
            {'a', J_PERM_UP},
            // {'b', ""},  // buffer piece is already in the correct position but not oriented
            {'c', J_PERM_DOWN},
            {'d', T_PERM},
            {'e', "R L F R' " + J_PERM_DOWN + " R F' L' R'"},
            {'f', "R F R' " + J_PERM_DOWN + " R F' R'"},
            {'g', "L' R F R' " + J_PERM_DOWN + " R F' R' L"},
            {'h', "U B' U' " + T_PERM + " U B U'"},
            {'i', "R2 U' R' F' R' " + J_PERM_DOWN + " R F R U R2"},
            {'j', "U2 R U2 " + T_PERM + " U2 R' U2"},
            {'k', "R F R' L' " + T_PERM + " L R F' R'"},
            {'l', "L' " + T_PERM + " L"},
            // {'m', ""},  // buffer piece is already in the correct position but not oriented
            {'n', "U B U' " + T_PERM + " U B' U'"},
            {'o', "D' R F R' L' " + T_PERM + " L R F' R' D"},
            {'p', "U' F' U " + T_PERM + " U' F U"},
            {'q', "R2 U R' F' R' " + J_PERM_DOWN + " R F R U' R2"},
            {'r', "U2 R' U2 " + T_PERM + " U2 R U2"},
            {'s', "D L R' B' R " + J_PERM_UP + " R' B L' R D'"},
            {'t', "L " + T_PERM + " L'"},
            {'u', "R F2 R' " + J_PERM_DOWN + " R F2 R'"},
            {'v', "D' R F2 R' " + J_PERM_DOWN + " R F2 R' D"},
            {'w', "R' B2 R " + J_PERM_UP + " R' B2 R"},
            {'x', "D R F2 R' " + J_PERM_DOWN + " R F2 R' D"}
};

/**
 * Translate current buffer piece letter to solving algorithm
 * */
const std::map<char, std::string> MOVE_FOR_SWAPPING_BUFFER_WITH_TARGET_CORNER = {
        // {'A', ""},  // buffer piece is already in the correct position but not oriented
        {'B', "U " + J_PERM_UP + " U'"},
        {'C', Y_PERM},
        {'D', "U2 " + J_PERM_DOWN + " U2"},
        // {'E', ""},  // buffer piece is already in the correct position but not oriented
        {'F', "F " + Y_PERM + " F'"},
        {'G', "D R " + Y_PERM + " R' D'"},
        {'H', "D2 F' " + Y_PERM + " F D2"},
        {'I', "F R U " + J_PERM_UP + " U' R' F'"},
        {'J', "R U " + J_PERM_UP + " U' R'"},
        {'K', "R " + Y_PERM + " R'"},
        {'L', "D F' " + Y_PERM + " F D'"},
        {'M', "F' U2 " + J_PERM_DOWN + " U2 F"},
        {'N', "R' F' U2 " + J_PERM_DOWN + " U2 F R"},
        {'O', "D' R " + Y_PERM + " R' D"},
        {'P', "F' " + Y_PERM + " F"},
        // {'Q', ""},  // buffer piece is already in the correct position but not oriented
        {'R', "R' " + Y_PERM + " R"},
        {'S', "D' F' " + Y_PERM + " F D"},
        {'T', "D2 R " + Y_PERM + " R' D2"},
        {'U', "F2 " + Y_PERM + " F2"},
        {'V', "D' F2 " + Y_PERM + " F2 D"},
        {'W', "R2 " + Y_PERM + " R2"},
        {'X', "D' F2 " + Y_PERM + " F2 D"}
};

/**
 * A array of all possible moves.
 * */
const std::array<std::string, 18> ALL_MOVES = {
    "F", "F'", "F2", "B", "B'", "B2", "U", "U'", "U2",
    "D", "D'", "D2", "L", "L'", "L2", "R", "R'", "R2"};

/**
 * Function that returns a random scramble
 * */
std::vector<std::string> getRandomShuffel(size_t n);

/**
 * MANIPULATION_TO_INT contains all allowed manipulations and there
 * corresponding id.
 * */
const std::map<char, int> MANIPULATION_TO_INT = {{'F', 0}, {'B', 1}, {'U', 2},
                                                 {'D', 3}, {'L', 4}, {'R', 5}};

/**
 * Splits a string on spaces.
 * */
std::vector<std::string>* rcsSplitString(std::string str);

/**
 * A class that represents a Rubik's Cube.
 * */
class Cube {
protected:
  /**
   * Nested array witch contains the current cube state.
   * The Rubik's Cube contains fo 6 Faces with each 3 Rows
   * and 3 Columns.
   * */
  BOARD board{};

public:
  Cube();
  ~Cube();

  /**
   * Returns a ptr to the current board.
   * */
  // BOARD* getBoard();

  /**
   * Return the selected Face of the Cube.
   * */
  FACE getFace(int a);

  /**
   * Update the current board.
   * TODO(me) how to check if a given board is solvable and valid
   * */
  // void setBoard(int* pBoard);

  /**
   * Get the element at a given index (x, y, z)
   * */
  int getBoardElement(int x, int y, int z);

  /**
   * Manipulate the Rubik's Cube Class with the given Cube notation.
   * For mor information about the notation refer to the README.md
   * */
  void manipulation(std::vector<std::string> const &instructions);

  /**
   * Atomic cube manipulations.
   * */

  // rotate the front face (blue) of the cube by 90⁰ clockwise
  void front();

  // rotate the back face (green) of the cube by 90⁰ counterclockwise
  void back();

  // rotate the up face (white) of the cube by 90⁰ clockwise
  void up();

  // rotate the down face (yellow) of the cube by 90⁰ clockwise
  void down();

  // rotate the left face (red) of the cube by 90⁰ clockwise
  void left();

  // rotate the right face (orange) of the cube by 90⁰ clockwise
  void right();
}; // end-class Cube

/**
 * Printing the current board of the cube.
 * */
std::ostream &operator<<(std::ostream &pOstream, Cube &c);

/**
 * Check if tow Cube objects are representing the same Cube.
 * @return true if cubes are eq else false
 * */
bool operator==(Cube a, Cube b);

} // namespace rubikscube

#endif // RUBIKS_CUBE_HH_
