// Copyright (C) 2022-now by Felix D | GNU GPLv2
// Created by felix on 12.10.2022 (dd.mm.yyyy)
//

#include <array>
#include <iostream>
#include <numeric>
#include <vector>

#include "./rubiks-cube.hh"

int main() {
  // create a cube obj
  rubikscube::Cube myCube = rubikscube::Cube();
  std::vector<std::string> randomShuffel = rubikscube::getRandomShuffel(10);
  myCube.manipulation(randomShuffel);

  std::cout << "Cube after applying ->"
            << std::accumulate(randomShuffel.begin() + 1, randomShuffel.end(),
                               randomShuffel[0],
                               [](const std::string &a, const std::string &b) {
                                 return a + " " + b;
                               })
            << "<-" << std::endl
            << "results in:" << std::endl
            << myCube << std::endl;

  rubikscube::Cube abc = rubikscube::Cube();

  // printing out the current element at a given point
  std::cout << "the element at (4,2,1) is: " << abc.getBoardElement(3, 1, 0)
            << std::endl;

  // printing all elements
  std::cout << "\nprinting the board:\n\n" << abc << std::endl;

  // performing manipulations
  std::cout << "performing manipulations: U B2 B'\n" << std::endl;
  std::vector<std::string> testMan = {"U", "B2", "D'"};
  // std::vector<std::string> testMan = {"F", "B2", "F'", "B2"};
  abc.manipulation(testMan);

  // printing all elements
  std::cout << "\nprinting the board:\n\n" << abc << std::endl;

  return 0;
}
