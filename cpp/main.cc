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

  // solve all center pieses
  std::vector<char> solutionMoves;
  // myCube.moveCenterBuffer2targetLocation(&solutionMoves);

  // check for parity
  // if the numebr of moves is odd, applie the parity fixing algorithm
  // (apply the R_PERM)
  if (solutionMoves.size() != 0 && solutionMoves.size() % 2 != 0) {
    // TODO myCube.translate(R PERM); and split R Perm into vect ...
    solutionMoves.push_back('#'); // indicate parity fix ...
  }

  // solve all corner pieces
  // myCube.moveCornerBuffer2TargetLocation(&solutionMoves);

  // DONE!!! myCube should be solved now ...

  // ===========================================================================

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
