-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module RandomMaze where
import Solver
import Data.HashMap as M
import Control.Monad
import Test.QuickCheck

-- Arbitrary MazeMap Generation
instance Arbitrary MazeMap where
  arbitrary = do 
    height <- genDimension
    width  <- genDimension
    genRandomMaze height width

readLines :: String -> IO [String]
readLines filePath =   do
                       s <- readFile filePath
                       return $ lines s

solveRandomMaze :: IO ()
solveRandomMaze = do
  putStrLn $ "Random Maze:\n"
  heights <- sample' genDimension
  let height = head heights
  widths <- sample' genDimension
  let width = head widths      
  mazes <- sample' $ genRandomMaze height width
  let maze = head mazes
  putStrLn $ showMaze (maze) height width
  let solvedMaze = solveMaze maze
  putStrLn $ "Solved Maze:\n"
  putStrLn $ showMaze (solvedMaze) height width

showRandomMaze' :: IO ()
showRandomMaze' = do
  heights <- sample' genDimension
  let height = head heights
  widths <- sample' genDimension
  let width = head widths      
  mazes <- sample' $ genRandomMaze height width
  putStrLn $ showMaze (head mazes) height width
  
genRandomMaze :: Int -> Int -> Gen MazeMap
genRandomMaze height width = do 
    spaces <- genRows height width
    sstart <- oneof [genStartRowPt, genStartColPt]
    end    <- oneof [genEndRowPt, genEndColPt]
    sol    <- randomPath sstart end
    let randomCells = insertNodes M.empty spaces Space
        solvable = insertNodes randomCells sol Space
        startable = insertNode solvable sstart Start
        endable = insertNode startable end End
    return $ endable
    where
      genStartRowPt = liftM ((,) 0) (choose (0, ((width `div` 2)-1)))
      genStartColPt = liftM (\ rrow -> (rrow, 0)) 
                      (choose (0, ((height `div` 2)-1)))
      genEndRowPt   = liftM ((,) (height-1)) 
                      (choose ((width `div` 2)-1, width-1))
      genEndColPt   = liftM (\ rrow -> (rrow, width-1)) 
                      (choose ((height `div` 2)-1, height-1))
    
randomPath :: (Int,Int) -> (Int,Int) -> Gen [(Int,Int)]
randomPath (sx, sy) (ex, ey) 
  | (sx == ex) && (sy == ey) = do
    return $ (ex, ey) : []
  | otherwise = do
    (nx, ny) <- gennxny
    liftM ((sx, sy) :) (randomPath (nx, ny) (ex, ey))
    where 
      gennx   = suchThat (liftM (sx+) 
                          (frequency [(4, return (-1)), (6, return 1)]))
                (\x -> (x <= ex) && (x > 0)) 
      genny   = suchThat (liftM (sy+) 
                          (frequency [(1, return (-1)), (3, return 1)]))
                (\y -> (y <= ey) && (y > 0))
      gennxny = oneof [genxny, gennxy]
      genxny  = liftM ((,) sx) genny
      gennxy  = liftM (\ x -> (x, sy)) gennx
      
genDimension :: Gen Int
genDimension = elements [15 .. 30]

genRows :: Int -> Int -> Gen [(Int, Int)]
genRows 0 width =  genRow 0 width
genRows height width = liftM2 (++) (genRows (height-1) width) 
                       (genRow height width)

genRow :: Int -> Int -> Gen[(Int, Int)]
genRow rrow 0     = do   
  cell <- genCell rrow 0
  return $ cell ++ []
genRow rrow width = liftM2 (++) (genCell rrow width) (genRow rrow (width-1))

genCell :: Int -> Int -> Gen [(Int, Int)]
genCell rrow col = frequency [ (7, return [])
                               , (3, return [(rrow,col)])]

-- Display Functions
showMaze :: MazeMap -> Int -> Int -> String
showMaze mazeMap height width = showRows mazeMap (height-1) (width-1)

showRows :: MazeMap -> Int -> Int -> String
showRows mazeMap 0      width = showRow mazeMap 0 width
showRows mazeMap height width = (showRows mazeMap (height-1) width) ++ 
                                '\n' : (showRow mazeMap height width)
                                
showRow :: MazeMap -> Int -> Int -> String
showRow mazeMap rrow 0     = showCell mazeMap rrow 0
showRow mazeMap rrow width =  (showRow mazeMap rrow (width-1)) ++ 
                             (showCell mazeMap rrow width)

showCell :: MazeMap -> Int -> Int -> String
showCell mazeMap rrow col = case (findWithDefault E (rrow,col) mazeMap) of
  (N _ Space False _) -> " "
  (N _ Space True _)  -> "+"
  (N _ Start _ _)     -> "S"
  (N _ End _ _)       -> "E"
  _                   -> "*"

getNodeType :: Node -> NodeType
getNodeType E = Wall
getNodeType (N _ nType _ _) = nType
