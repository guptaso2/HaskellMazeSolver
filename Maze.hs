-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Data.HashMap as M
import Data.List as L
import Data.List.Key as K

main :: IO ()
main = do 
  s <- readLines "test.txt"
  putStrLn $ show s

data NodeType = Start | End | Space
                deriving Show

data DirectionNode = Ed
                     | Nd (Int,Int)
                     deriving Show

data Node = E               -- Empty Node
            | N             -- Non-empty Node
                (Int, Int)  -- Coordiates of node
                NodeType    -- Specifies if node is start, end or space
                Bool        -- Contains solution path
                [(Int,Int)]
            deriving Show

type MazeMap = Map (Int,Int) Node


insertNode :: MazeMap -> (Int,Int) -> NodeType -> MazeMap
insertNode mazeMap (x,y) nType = M.insert (x,y) node rightMap where
                   rightMap  = updateMazeMap leftMap rightNode
                   leftMap   = updateMazeMap downMap leftNode
                   downMap   = updateMazeMap upMap downNode
                   upMap     = updateMazeMap mazeMap upNode
                   node      = N (x,y) nType False neighbors
                   neighbors = makeNeighborList [upNode, downNode, leftNode, rightNode]
                   upNode    = updateNode (findWithDefault E (x-1,y) mazeMap) (x,y)
                   downNode  = updateNode (findWithDefault E (x+1,y) mazeMap) (x,y)
                   leftNode  = updateNode (findWithDefault E (x,y-1) mazeMap) (x,y)
                   rightNode = updateNode (findWithDefault E (x,y+1) mazeMap) (x,y)

makeNeighborList::[Node] -> [(Int,Int)]
makeNeighborList l = L.map g $ L.filter f l where
                 f E           = False
                 f (N _ _ _ _) = True
                 g (N x _ _ _) = x
                 g _           = undefined

updateNode :: Node -> (Int,Int) -> Node
updateNode E _ = E
updateNode (N coords nType sol n) (x,y) = (N coords nType sol ((x,y):n))

updateMazeMap :: MazeMap -> Node -> MazeMap
updateMazeMap mazeMap E = mazeMap
updateMazeMap mazeMap node@(N coords _ _ _) = M.insert coords node mazeMap

getCoords :: Char -> [String] -> [(Int,Int)]
getCoords c ss = aux c ss 0 where
          aux _ []      _ = []
          aux cc (x:xs) n = (getColNumbers cc x n) ++ (aux cc xs (n+1))

getColNumbers :: Char -> String -> Int -> [(Int,Int)]
getColNumbers c s lineNum = L.map (\x -> (lineNum,x)) $ elemIndices c s

readLines :: String -> IO [String]
readLines filePath =   do
                       s <- readFile filePath
                       return $ lines s

{--dijkstra :: (Int,Int) -> MazeMap -> Map (Int,Int) (Int, Maybe (Int,Int))
dijkstra source@(sx,sy) graph =
         f (fromList [(v, (if v == source then 0 else 1/0, Nothing))
                          | v <- keys graph]) (keys graph) where
         f ds [] = ds
         f ds q = f (foldr relax ds graph ! m) (delete m q) where
                  m = K.minimum (fst . (ds !)) q --}