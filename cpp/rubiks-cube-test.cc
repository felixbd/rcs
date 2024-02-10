// Copyright (C) 2022-2024 by Felix D | GNU GPLv2
// Created by felix on 11.10.2022 (dd.mm.yyyy)
//

#include <gtest/gtest.h>
// #include <vector>

#include "./rubiks-cube.hh"

///////////////////////////////////////////////////////////////////////////////
// Test for printing faces of a cube
///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeHelperTest, rubikscubePrintFace) {
  rubikscube::FACE x = {{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  ASSERT_EQ(rubikscube::printFace(x), "123\n456\n789\n")
      << "printing cube face";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeHelperTest, rubikscubeRotateMatrix) {
  rubikscube::FACE a = {{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}};

  rubikscube::FACE b = {{{7, 4, 1}, {8, 5, 2}, {9, 6, 3}}};

  ASSERT_EQ(a, rubikscube::rotateMatrix(a, 4)) << "rotating matrix (4x)";

  ASSERT_EQ(rubikscube::rotateMatrix(a, 1), b) << "rotating matrix (1x)";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeTest, checkCubeConstructor) {
  ASSERT_NO_THROW(rubikscube::Cube c1 = rubikscube::Cube();)
      << "construction of object from class Cube";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeTest, checkEq) {
  // create cube obj
  rubikscube::Cube c4 = rubikscube::Cube();
  rubikscube::Cube c5 = rubikscube::Cube();

  EXPECT_TRUE(c4 == c5) << "solved qubes should be equal";

  // manipulate Cube c4
  std::vector<std::string> instruction = {"R", "U", "D"};
  c4.manipulation(instruction);

  EXPECT_FALSE(c4 == c5) << "not eq cubes should not be eq";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, front) {
  // create cube obj
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the front face clockwise
  c1.front();

  // check if manipulation was done correctly
  rubikscube::FACE frontFace = {{{2, 2, 2}, {2, 2, 2}, {2, 2, 2}}};

  rubikscube::FACE backFace = {{{4, 4, 4}, {4, 4, 4}, {4, 4, 4}}};

  rubikscube::FACE upFace = {{
      {0, 0, 0}, {0, 0, 0}, {1, 1, 1} // from the left to the top
  }};

  rubikscube::FACE downFace = {{{3, 3, 3}, // from the right to the bottom
                                {5, 5, 5},
                                {5, 5, 5}}};

  rubikscube::FACE leftFace = {{{1, 1, 5}, {1, 1, 5}, {1, 1, 5}}};

  rubikscube::FACE rightFace = {{{0, 3, 3}, {0, 3, 3}, {0, 3, 3}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should not be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, back) {
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the back face counterclockwise
  c1.back();

  // check if manipulation was done correctly
  rubikscube::FACE frontFace = {{{2, 2, 2}, {2, 2, 2}, {2, 2, 2}}};

  rubikscube::FACE backFace = {{{4, 4, 4}, {4, 4, 4}, {4, 4, 4}}};

  rubikscube::FACE upFace = {{{3, 3, 3}, // from the right to the top
                              {0, 0, 0},
                              {0, 0, 0}}};

  rubikscube::FACE downFace = {{{5, 5, 5}, // from the left to the bottom
                                {5, 5, 5},
                                {1, 1, 1}}};

  rubikscube::FACE leftFace = {{{0, 1, 1}, {0, 1, 1}, {0, 1, 1}}};

  rubikscube::FACE rightFace = {{{3, 3, 5}, {3, 3, 5}, {3, 3, 5}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should not be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, top) {
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the top face
  c1.up();

  // check if manipulation was done correctly
  rubikscube::FACE upFace = {{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}};

  rubikscube::FACE downFace = {{{5, 5, 5}, {5, 5, 5}, {5, 5, 5}}};

  rubikscube::FACE leftFace = {{{2, 2, 2}, // from front to left
                                {1, 1, 1},
                                {1, 1, 1}}};

  rubikscube::FACE frontFace = {{{3, 3, 3}, {2, 2, 2}, {2, 2, 2}}};

  rubikscube::FACE rightFace = {{{4, 4, 4}, {3, 3, 3}, {3, 3, 3}}};

  rubikscube::FACE backFace = {{{1, 1, 1}, {4, 4, 4}, {4, 4, 4}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should not be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, down) {
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the bottom face
  c1.down();

  // check if manipulation was done correctly
  rubikscube::FACE upFace = {{{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}};

  rubikscube::FACE downFace = {{{5, 5, 5}, {5, 5, 5}, {5, 5, 5}}};

  rubikscube::FACE leftFace = {{
      {1, 1, 1}, {1, 1, 1}, {4, 4, 4} // from back to left
  }};

  rubikscube::FACE frontFace = {{{2, 2, 2}, {2, 2, 2}, {1, 1, 1}}};

  rubikscube::FACE rightFace = {{{3, 3, 3}, {3, 3, 3}, {2, 2, 2}}};

  rubikscube::FACE backFace = {{{4, 4, 4}, {4, 4, 4}, {3, 3, 3}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should not be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, left) {
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the left face
  c1.left();

  // check if manipulation was done correctly
  rubikscube::FACE leftFace = {{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}};

  rubikscube::FACE rightFace = {{{3, 3, 3}, {3, 3, 3}, {3, 3, 3}}};

  rubikscube::FACE upFace = {{{4, 0, 0}, {4, 0, 0}, {4, 0, 0}}};

  rubikscube::FACE downFace = {{{2, 5, 5}, {2, 5, 5}, {2, 5, 5}}};

  rubikscube::FACE frontFace = {{{0, 2, 2}, {0, 2, 2}, {0, 2, 2}}};

  rubikscube::FACE backFace = {{{5, 4, 4}, {5, 4, 4}, {5, 4, 4}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should not be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, right) {
  rubikscube::Cube c1 = rubikscube::Cube();

  // rotate the right face
  c1.right();

  // check if manipulation was done correctly
  rubikscube::FACE leftFace = {{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}}};

  rubikscube::FACE rightFace = {{{3, 3, 3}, {3, 3, 3}, {3, 3, 3}}};

  rubikscube::FACE upFace = {{{0, 0, 2}, {0, 0, 2}, {0, 0, 2}}};

  rubikscube::FACE downFace = {{{5, 5, 4}, {5, 5, 4}, {5, 5, 4}}};

  rubikscube::FACE frontFace = {{{2, 2, 5}, {2, 2, 5}, {2, 2, 5}}};

  rubikscube::FACE backFace = {{{4, 4, 0}, {4, 4, 0}, {4, 4, 0}}};

  EXPECT_EQ(c1.getFace(0), upFace) << "up face should be rotated";
  EXPECT_EQ(c1.getFace(1), leftFace) << "left face should be rotated";
  EXPECT_EQ(c1.getFace(2), frontFace) << "front face should not be rotated";
  EXPECT_EQ(c1.getFace(3), rightFace) << "right face should be rotated";
  EXPECT_EQ(c1.getFace(4), backFace) << "back face should be rotated";
  EXPECT_EQ(c1.getFace(5), downFace) << "down face should be rotated";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeManipulationTest, checkCubeManipulation) {
  // create cube obj
  rubikscube::Cube c2 = rubikscube::Cube();
  rubikscube::Cube c3 = rubikscube::Cube();

  // manipulate the cube c2 with a Sexy Move
  std::vector<std::string> sexyMove = {"R", "U", "R'", "U'"};
  c2.manipulation(sexyMove);

  // TODO(me) check if manipulation was done correctly

  // manipulate the cube c2 with a reverse Sexy Move
  std::vector<std::string> reverseSexyMove = {"U", "R", "U'", "R'"};
  c2.manipulation(reverseSexyMove);

  // check if manipulation was done correctly (cube should eq a solved qube)
  EXPECT_TRUE(c2 == c3) << "c2 should eq a solved Cube (c3)";

  // TODO(me) test some more permutations ...
  // T Perm: RUR'UR'FR2U'R'U'RUR'F'
  // J Perm: R'UL'U2RU'R'U2RL
}

///////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
