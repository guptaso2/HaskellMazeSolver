-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Data.HashMap
import Data.List

main :: IO ()
main = do 
  s <- readLines "test.txt"
  putStrLn $ show s

data NodeType = Start | End | Space
                deriving Show

data Direction = U | D | L | R
                 deriving Show

data DirectionNode = Ed
                     | Nd (Int,Int)
                     deriving Show

data Node = E               -- Empty Node
            | N             -- Non-empty Node
                (Int, Int)  -- Coordiates of node
                NodeType    -- Specifies if node is start, end or space
                Bool        -- Contains solution path
                DirectionNode        -- DirectionNode bordering Up direction
                DirectionNode        -- DirectionNode bordering Down direction         
                DirectionNode        -- DirectionNode bordering Left direction
                DirectionNode        -- DirectionNode bordering Right direction
            deriving Show

type MazeMap = Map (Int,Int) Node


insertNode :: MazeMap -> (Int,Int) -> NodeType -> MazeMap
insertNode mazeMap (x,y) nType = Data.HashMap.insert (x,y) node rightMap where
                   rightMap  = updateMazeMap leftMap rightNode
                   leftMap   = updateMazeMap downMap leftNode
                   downMap   = updateMazeMap upMap downNode
                   upMap     = updateMazeMap mazeMap upNode
                   node      = N (x,y) nType False upNodeD downNodeD leftNodeD rightNodeD
                   upNodeD    = getDirectionNode $ upNode
                   downNodeD  = getDirectionNode $ downNode
                   leftNodeD  = getDirectionNode $ leftNode
                   rightNodeD = getDirectionNode $ rightNode
                   upNode    = updateNode (findWithDefault E (x-1,y) mazeMap) (x,y) D
                   downNode  = updateNode (findWithDefault E (x+1,y) mazeMap) (x,y) U
                   leftNode  = updateNode (findWithDefault E (x,y-1) mazeMap) (x,y) R
                   rightNode = updateNode (findWithDefault E (x,y+1) mazeMap) (x,y) L

getDirectionNode :: Node -> DirectionNode
getDirectionNode E                 = Ed
getDirectionNode (N x _ _ _ _ _ _) = Nd x

updateNode :: Node -> (Int,Int) -> Direction -> Node
updateNode E _ _ = E
updateNode (N coords nType sol _ d l r) (x,y) U = (N coords nType sol (Nd (x,y)) d l r) 
updateNode (N coords nType sol u _ l r) (x,y) D = (N coords nType sol u (Nd (x,y)) l r) 
updateNode (N coords nType sol u d _ r) (x,y) L = (N coords nType sol u d (Nd (x,y)) r) 
updateNode (N coords nType sol u d l _) (x,y) R = (N coords nType sol u d l (Nd (x,y))) 

updateMazeMap :: MazeMap -> Node -> MazeMap
updateMazeMap mazeMap E = mazeMap
updateMazeMap mazeMap node@(N coords _ _ _ _ _ _) = Data.HashMap.insert coords node mazeMap

getCoords :: Char -> [String] -> [(Int,Int)]
getCoords c ss = aux c ss 0 where
          aux _ []      _ = []
          aux cc (x:xs) n = (getColNumbers cc x n) ++ (aux cc xs (n+1))

getColNumbers :: Char -> String -> Int -> [(Int,Int)]
getColNumbers c s lineNum = Data.List.map (\x -> (lineNum,x)) $ elemIndices c s

readLines :: String -> IO [String]
readLines filePath =   do
                       s <- readFile filePath
                       return $ lines s