// Copyright (C) 2022-2024 by Felix D | GNU GPLv2
// Created by felix on 12.10.2022 (dd.mm.yyyy)
//

#include <array>
#include <chrono> // NOLINT [build/c++11]
#include <iostream>
#include <numeric>
#include <vector>

#include "./rubiks-cube.hh"

int main() {
  // measure time of function execution
  std::chrono::high_resolution_clock::time_point start;
  std::chrono::high_resolution_clock::time_point end;
  std::chrono::duration<double> duration;

  // create a cube obj
  rubikscube::Cube myCube = rubikscube::Cube();

  // get scramble and apply it
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

  // Start time
  start = std::chrono::high_resolution_clock::now();

  // solve all center pieses
  std::vector<std::string> solutionMoves;
  myCube.moveCenterBuffer2targetLocation(&solutionMoves);

  // check for parity
  // if the numebr of moves is odd, applie the parity fixing algorithm
  // (apply the R_PERM)
  if (solutionMoves.size() != 0 && solutionMoves.size() % 2 != 0) {
    std::string move = rubikscube::R_PERM;

    std::vector<std::string> tokens;
    std::stringstream ss(move);
    std::string token;

    while (ss >> token) {
      tokens.push_back(token);
    }

    myCube.manipulation(tokens);
    solutionMoves.push_back(" - parity fix - ");
  }

  // solve all corner pieces
  myCube.moveCornerBuffer2targetLocation(&solutionMoves);

  // end time (cube is solved now ...)
  end = std::chrono::high_resolution_clock::now();
  duration = end - start;

  std::cout << "total time: " << duration.count() << std::endl;

  // DONE!!! myCube should be solved now ...
  std::cout << "\nsolution:\n" << std::endl;
  for (const auto &str : solutionMoves) {
    std::cout << str << " ";
  }

  std::cout << "\n\ncube:\n" << myCube << std::endl;

  // ===========================================================================

  /*
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

  */

  return EXIT_SUCCESS;
}
