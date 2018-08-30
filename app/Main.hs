module Main where

import Sudoku

main :: IO ()
main = do input <- getLine
          let grid = groupn 9 input
              results = sudoku grid 
          traverse (putStrLn . output) results
          pure ()
