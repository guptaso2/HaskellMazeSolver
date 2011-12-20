-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module Maze where
import Solver
import RandomMaze

main :: IO ()
main
  = cmdLine

cmdLine :: IO ()
cmdLine = do
  putStrLn $ "Please enter a map filename:"
  filename <- getLine
  s <- readLines filename
  let height = length s
  let width = length $ head s
  let mazeMap = buildMazeMap s 
  putStrLn $ "Maze:\n" ++ (showMaze mazeMap height width)
  let solvedMaze = solveMaze mazeMap
  putStrLn $ "Solution:\n" ++ (showMaze solvedMaze height width)

                       
  
