// Copyright (C) 2022-now by Felix D | GNU GPLv2
// Created by felix on 12.10.2022 (dd.mm.yyyy)
//

#include <array>
#include <map>
#include <vector>
#include <iostream>
#include <string>
#include <stdexcept>

#include "./rubiks-cube.hh"

namespace rubikscube {

////////////////////////////////////////////////////////////////////////////////
std::string printFace(FACE face) {
    std::string rvString;

    for (int x = 0; x < 3; x++) {
        for (int y = 0; y < 3; y++) {
          rvString += std::to_string(face[x][y]);
        }
        rvString += '\n';
    }
    return rvString;
}

////////////////////////////////////////////////////////////////////////////////
FACE rotateMatrix(FACE a, int num) {
    for (int index = 0; index < num; index++) {
        // Transposing the matrix
        for (int i = 0; i < 3; i++) {
            for (int j = i + 1; j < 3; j++) {
                std::swap(a[i][j], a[j][i]);
            }
        }
        // Reversing each row of the matrix
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3 / 2; j++) {
                std::swap(a[i][j], a[i][3-j-1]);
            }
        }
    }
    return a;
}

////////////////////////////////////////////////////////////////////////////////
Cube::Cube() {
    board = SOLVED_BOARD;
}

////////////////////////////////////////////////////////////////////////////////
Cube::~Cube() = default;

////////////////////////////////////////////////////////////////////////////////
FACE Cube::getFace(int a) {
    if (a < 0 || a > 5) {
        throw std::invalid_argument("INDEX OF getFace out of bound");
    }
    return board[a];
}

////////////////////////////////////////////////////////////////////////////////
int Cube::getBoardElement(int x, int y, int z) {
    if (x < 0 || x > 5) {
        throw std::invalid_argument("INDEX OF getBordElement x out of bound");
    } else if (y < 0 || y > 2) {
        throw std::invalid_argument("INDEX OF getBordElement y out of bound");
    } else if (z < 0 || z > 2) {
        throw std::invalid_argument("INDEX OF getBordElement z out of bound");
    }

    return board[x][y][z];
}

////////////////////////////////////////////////////////////////////////////////
std::ostream& operator<<(std::ostream& pOstream, Cube& c) {
    return pOstream << printFace(c.getFace(0))
                    << "---\n"
                    << printFace(c.getFace(1))
                    << "---\n"
                    << printFace(c.getFace(2))
                    << "---\n"
                    << printFace(c.getFace(3))
                    << "---\n"
                    << printFace(c.getFace(4))
                    << "---\n"
                    << printFace(c.getFace(5))
                    << std::endl;
}

////////////////////////////////////////////////////////////////////////////////
bool operator==(Cube a, Cube b) {
    for (int x = 0; x < 6; x++) {
      for (int y = 0; y < 3; y++) {
        for (int z = 0; z < 3; z++) {
          if (a.getBoardElement(x, y, z) != b.getBoardElement(x, y, z)) {
                    return false;
          }
        }
      }
    }
    return true;
}

////////////////////////////////////////////////////////////////////////////////
void Cube::manipulation(std::vector<std::string> const& instructions) {
    // the number of times each manipulation should be performed
    int num;

    // loop over every instruction
    for (const std::string& instruction : instructions) {
        num = (instruction.length() == 1) ? 1 :
                (instruction[1] == '2') ? 2 : 3;
        // perform manipulation (n times)
        for (int n = 0; n < num; n++) {
            switch (MANIPULATION_TO_INT.at(instruction[0])) {
                case 0: front(); break;
                case 1: back();  break;
                case 2: up();    break;
                case 3: down();  break;
                case 4: left();  break;
                case 5: right(); break;
                default: std::cerr << "INVALID OP" << std::endl; break;
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
void Cube::front() {
    // get the current relevant values
    ROW whiteRow  = board[0][2];
    ROW yellowRow = board[5][0];
    ROW orangeRow = { board[3][0][0], board[3][1][0], board[3][2][0] };
    ROW redRow    = { board[1][0][2], board[1][1][2], board[1][2][2] };

    // update the cube by rotating the blue face clockwise by 90°
    board[0][2] = {redRow[2], redRow[1], redRow[0]};
    for (int i = 0; i < 3; i++) { board[3][i][0] = whiteRow[i]; }
    board[5][0] = {orangeRow[2], orangeRow[1], orangeRow[0]};
    for (int i = 0; i < 3; i++) { board[1][i][2] = yellowRow[i]; }
    board[2] = rotateMatrix(board[2], 1);
}

////////////////////////////////////////////////////////////////////////////////
void Cube::back() {
    // get the current relevant values
    ROW whiteRow =  { board[0][0][2], board[0][0][1], board[0][0][0] };
    ROW orangeRow = { board[3][0][2], board[3][1][2], board[3][2][2] };
    ROW yellowRow = { board[5][2][2], board[5][2][1], board[5][2][0] };
    ROW redRow =    { board[1][0][0], board[1][1][0], board[1][2][0] };

    // update the cube by rotating the blue face clockwise by 90°
    board[0][0] = orangeRow;
    for (int i = 0; i < 3; i++) { board[3][i][2] = yellowRow[i]; }
    board[5][2] = redRow;
    for (int i = 0; i < 3; i++) { board[1][i][0] = whiteRow[i]; }
    board[4] = rotateMatrix(board[4], 3);
}

////////////////////////////////////////////////////////////////////////////////
void Cube::up() {
    // get the current state of the cube
    ROW blueRow   = board[2][0];
    ROW redRow    = board[1][0];
    ROW greenRow  = board[4][0];
    ROW orangeRow = board[3][0];

    // update the cube by rotating the white face clockwise by 90°
    board[2][0] = orangeRow;
    board[1][0] = blueRow;
    board[4][0] = { redRow[2], redRow[1], redRow[0] };
    board[3][0] = { greenRow[2], greenRow[1], greenRow[0] };
    board[0] = rotateMatrix(board[0], 1);
}

////////////////////////////////////////////////////////////////////////////////
void Cube::down() {
    // get the current state of the cube
    ROW blueRow   = board[2][2];
    ROW redRow    = board[1][2];
    ROW greenRow  = board[4][2];
    ROW orangeRow = board[3][2];

    // update the cube by rotating the yellow face clockwise by 90°
    board[2][2] = redRow;
    board[3][2] = blueRow;
    board[4][2] = { orangeRow[2], orangeRow[1], orangeRow[0] };
    board[1][2] = { greenRow[2], greenRow[1], greenRow[0] };
    board[5] = rotateMatrix(board[5], 1);
}

////////////////////////////////////////////////////////////////////////////////
void Cube::left() {
    // get the current state of the cube
    ROW whiteRow =  { board[0][0][0], board[0][1][0], board[0][2][0] };
    ROW blueRow =   { board[2][0][0], board[2][1][0], board[2][2][0] };
    ROW yellowRow = { board[5][2][0], board[5][1][0], board[5][0][0] };
    ROW greenRow =  { board[4][2][0], board[4][1][0], board[4][0][0] };

    // update the cube by rotating the red face clockwise by 90°
    for (int i = 0; i < 3; i++) { board[0][i][0] = greenRow[i]; }
    for (int i = 0; i < 3; i++) { board[2][i][0] = whiteRow[i]; }
    for (int i = 0; i < 3; i++) { board[5][i][0] = blueRow[i]; }
    for (int i = 0; i < 3; i++) { board[4][i][0] = yellowRow[i]; }
    board[1] = rotateMatrix(board[1], 1);
}

////////////////////////////////////////////////////////////////////////////////
void Cube::right() {
    // get the current state of the cube
    ROW whiteRow =  { board[0][2][2], board[0][1][2], board[0][0][2] };
    ROW greenRow =  { board[4][2][2], board[4][1][2], board[4][0][2] };
    ROW yellowRow = { board[5][0][2], board[5][1][2], board[5][2][2] };
    ROW blueRow =   { board[2][0][2], board[2][1][2], board[2][2][2] };

    // update the cube by rotating the orange face clockwise by 90°
    for (int i = 0; i < 3; ++i) { board[0][i][2] = blueRow[i]; }
    for (int i = 0; i < 3; ++i) { board[4][i][2] = whiteRow[i]; }
    for (int i = 0; i < 3; ++i) { board[5][i][2] = greenRow[i]; }
    for (int i = 0; i < 3; ++i) { board[2][i][2] = yellowRow[i]; }
    board[3] = rotateMatrix(board[3], 1);
}

}  // namespace rubikscube
