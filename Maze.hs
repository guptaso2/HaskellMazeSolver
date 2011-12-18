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
  putStrLn $ "Rows:\n" ++ (show s)
  let height = length s
  let width = length $ head s
  let mazeMap = buildMazeMap s 
  putStrLn $ "MazeMap:\n" ++ (show mazeMap)
  putStrLn $ "Maze:\n" ++ (showMaze mazeMap height width)

data NodeType = Start | End | Space | Wall
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

showMaze :: MazeMap -> Int -> Int -> String
showMaze mazeMap height width = showRows mazeMap (height-1) (width-1)

showRows :: MazeMap -> Int -> Int -> String
showRows mazeMap 0      width = showRow mazeMap 0 width
showRows mazeMap height width = (showRows mazeMap (height-1) width) ++ 
                                '\n' : (showRow mazeMap height width)
                                
showRow :: MazeMap -> Int -> Int -> String
showRow mazeMap row 0     = showCell mazeMap row 0
showRow mazeMap row width =  (showRow mazeMap row (width-1)) ++ (showCell mazeMap row width)

showCell :: MazeMap -> Int -> Int -> String
showCell mazeMap row col = case nType of
  Space -> " "
  Start -> "S"
  End   -> "E"
  Wall  -> "*"
  where
    nType = getNodeType $ findWithDefault E (row,col) mazeMap
  
getNodeType :: Node -> NodeType
getNodeType E = Wall
getNodeType (N _ nType _ _ _ _ _) = nType


buildMazeMap :: [String] -> MazeMap
buildMazeMap rows = mazeMap
  where
    start = insertNodes empty (getCoords 'S' rows) Start
    spaces = insertNodes start (getCoords ' ' rows) Space
    mazeMap = insertNodes spaces (getCoords 'E' rows) End

insertNodes :: MazeMap -> [(Int,Int)] -> NodeType -> MazeMap
insertNodes mm coords n = foldr helper mm coords
  where helper coord m = insertNode m coord n

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
