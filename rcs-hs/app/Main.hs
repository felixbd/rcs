module Main (main) where

-- import Lib (someFunc)

import Cube (RubiksCube
            , newCube
            , possibleMoves
            , translateMove
            , applyMove)


-- applyMove :: [String] -> RubiksCube -> RubiksCube

main :: IO ()
main = do
  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"
  let myCube = newCube
  print myCube

  ----------------------------------------------------------------------
  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"

  -- apply multiple moves (should be solved by the end ...)
  let fizbuz = words "R U R' U' R' F R2 U' R' U' R' U' R U R' F' U' R' U L' U2 R U' R' U2 L R R2 U' R' F' R' R U R' F' R U R' U' R' F R2 U' R' U' R F R U R2 U2 R' U2 R U R' U' R' F R2 U' R' U' R U R' F' U2 R U2 D' R F2 R' R U R' F' R U R' U' R' F R2 U' R' U' R F2 R' D R F R' R U R' F' R U R' U' R' F R2 U' R' U' R F' R' R U R' U' R' F R2 U' R' U' R U R' F' U' R' U L' U2 R U' R' U2 L R R U' R' U' R U R D R' U' R D' R' U2 R' U' F R U U' R' U L' U2 R U' R' U2 L R U' R' F' D' F' F R U' R' U' R U R' F' R U R' U' R' F R F' F D F2 F R U' R' U' R U R' F' R U R' U' R' F R F' F2 R' F' U2 R U R' F' R U R' U' R' F R2 U' R' U' U2 F R F R U' R' U' R U R' F' R U R' U' R' F R F'"
  let fiz = applyMove fizbuz myCube

  print fiz

  putStrLn $ "\n" ++ concat (replicate 40 "= ") ++ "=\n"

  putStrLn $ "all moves: " ++ foldr (\x acc -> x ++ " " ++ acc) "" possibleMoves
