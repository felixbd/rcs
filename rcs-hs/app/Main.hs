module Main (main) where

-- import Lib (someFunc)

import Cube (RubiksCube
            , newCube
            , possibleMoves
            , translateMove)


main :: IO ()
main = do
  let myCube = newCube

  print myCube

  -- translateMove :: String -> (RubiksCube -> RubiksCube)
  let cubeFunc = translateMove "L"

  let fuu = cubeFunc myCube

  print fuu

  putStrLn $ concat possibleMoves
