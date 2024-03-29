-- Solver.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module Solver where
import Data.HashMap as M
import Data.List as L
import Data.List.Key as K

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

-- Construction Functions
buildMazeMap :: [String] -> MazeMap
buildMazeMap rows = mazeMap
  where
    sstart = insertNodes M.empty (getCoords 'S' rows) Start
    spaces = insertNodes sstart (getCoords ' ' rows) Space
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
                   neighbors = makeNeighborList 
                               [upNode, downNode, leftNode, rightNode]
                   upNode    = updateNode 
                               (findWithDefault E (x-1,y) mazeMap) (x,y)
                   downNode  = updateNode 
                               (findWithDefault E (x+1,y) mazeMap) (x,y)
                   leftNode  = updateNode 
                               (findWithDefault E (x,y-1) mazeMap) (x,y)
                   rightNode = updateNode 
                               (findWithDefault E (x,y+1) mazeMap) (x,y)

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

-- Solver Functions
dijkstra :: (Int,Int) -> MazeMap -> Map (Int,Int) (Float, Maybe (Int,Int))
dijkstra source mazemap =
         f (fromList [(v, (if v == source then 0 else 1/0, Nothing))
                          | v <- keys graph]) (keys graph) where
         graph = fromList[(v, nodesToCoords $ mazemap ! v) | v <- keys mazemap]
         f ds [] = ds
         f ds q = f (foldr relax ds $ graph ! m) (L.delete m q) where
                  m = K.minimum (fst . (ds !)) q
                  relax (e,d) = adjust (min (fst (ds ! m) + d, Just m)) e

shortestPath :: (Int,Int) -> (Int, Int) -> MazeMap -> [(Int,Int)]
shortestPath from to mazemap = reverse $ f to where
             f x = x : maybe [] f (snd $ dijkstra from mazemap ! x)

solveMaze :: MazeMap -> MazeMap
solveMaze mazemap = foldr (\k m -> adjust markAsSolution k m) mazemap sol where
          sol = shortestPath sstart end mazemap where
              sstart = head $ L.filter (\x -> isStart $ mazemap ! x) 
                      $ keys mazemap
              end   = head $ L.filter (\x -> isEnd $ mazemap ! x) 
                      $ keys mazemap

isStart :: Node -> Bool
isStart (N _ Start _ _) = True
isStart _               = False

isEnd :: Node -> Bool
isEnd   (N _ End   _ _) = True
isEnd   _               = False

markAsSolution :: Node -> Node
markAsSolution E = E
markAsSolution (N coords ntype _ neighbors) = (N coords ntype True neighbors)

nodesToCoords :: Node -> [((Int,Int),Float)]
nodesToCoords (N _ _ _ x) = L.map (\n -> (n,1)) x
nodesToCoords E = []
