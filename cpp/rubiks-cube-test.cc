// Copyright (C) 2022-now by Felix D | GNU GPLv2
// Created by felix on 11.10.2022 (dd.mm.yyyy)
//

#include <gtest/gtest.h>
#include <vector>
#include "./rubiks-cube.hh"

///////////////////////////////////////////////////////////////////////////////
// Test for printing faces of a cube
///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeHelperTest, rubikscubePrintFace) {
    rubikscube::FACE x = {{
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    }};

    ASSERT_EQ(rubikscube::printFace(x), "123\n456\n789\n")
        << "printing cube face";
}

///////////////////////////////////////////////////////////////////////////////
TEST(MainCubeHelperTest, rubikscubeRotateMatrix) {
    rubikscube::FACE a = {{
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    }};

    rubikscube::FACE b = {{
        {7, 4, 1},
        {8, 5, 2},
        {9, 6, 3}
    }};

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
// TODO(me) ...
TEST(MainCubeManipulationTest, front) {}
TEST(MainCubeManipulationTest, back) {}
TEST(MainCubeManipulationTest, top) {}
TEST(MainCubeManipulationTest, down) {}
TEST(MainCubeManipulationTest, left) {}
TEST(MainCubeManipulationTest, right) {}

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
