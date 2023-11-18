// Copyright (C) 2022-now by Felix D | GNU GPLv2
// Created by felix on 12.10.2022 (dd.mm.yyyy)
//


#include <array>
#include <vector>
#include <iostream>
#include <string>

#include "./rubiks-cube.hh"

int main() {
    // create a cube obj
    rubikscube::Cube abc = rubikscube::Cube();

    // printing out the current element at a given point
    std::cout << "the element at (4,2,1) is: "
              << abc.getBoardElement(2, 1, 0)
              << std::endl;

    // printing all elements
    std::cout << "\nprinting the board:\n\n" << abc << std::endl;

    // performing manipulations
    std::cout << "performing manipulations: U B2 B'\n" << std::endl;
    std::vector<std::string> testMan = {"U", "B2", "D'"};
    abc.manipulation(testMan);

    // printing all elements
    std::cout << "\nprinting the board:\n\n" << abc << std::endl;

    return 0;
}
