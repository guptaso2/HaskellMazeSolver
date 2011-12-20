-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module Main where
import Data.HashMap as M
import Data.List as L
import Data.List.Key as K
import Control.Monad
import Test.QuickCheck
import Graphics.UI.WX as G

cellDim :: Int
cellDim = 15

defPuzWidth :: Int
defPuzWidth = 35

defPuzHeight :: Int
defPuzHeight = 17

main :: IO ()
main
  = start mazeGui

mapFiles :: [(String,[String])]
mapFiles
   = [("Text Files",["*.txt"])]

mazeGui :: IO ()
mazeGui
  = do -- the application frame
       f <- frame [text := "Maze Solver!", clientSize := sz 600 600]
       mazemap <- variable [value := Nothing]
       ht <- variable [value := 0]
       wt <- variable [value := 0]

       sw <- scrolledWindow f [ on paint := onPaint mazemap ht wt
                              , virtualSize := sz 500 320, scrollRate := sz 10 10
                              , fullRepaintOnResize := False]

       quitBtn <- button f [text := "Quit", on command := close f]
       loadBtn <- button f [text := "Open Maze", on command := onOpen f sw mazemap ht wt]
       solveBtn <- button f [text := "Solve Maze", on command := solveIt sw mazemap]
       rndmBtn <- button f [text := "Random", on command := randMaze sw mazemap ht wt]

       set f [layout := (column 2 [(row 3 [floatCentre(widget quitBtn),
                                   floatCentre(widget loadBtn),
                                   floatCenter(widget solveBtn),
                                   floatCenter(widget rndmBtn)]),
                                   floatCentre(minsize (sz 527 330) $ fill $ widget sw)])]       
       where
        onOpen f sw mazemap ht wt
          = do mbfname <- fileOpenDialog f False True "Open image" mapFiles "" ""
               case mbfname of
                    Nothing    -> return ()
                    Just fname -> openMaze sw mazemap ht wt fname
        
        openMaze sw mazemap ht wt fname
          = do
                s <- readLines fname
                set ht [value := (length s)]
                set wt [value := (length $ head s)]
                set mazemap [value := Just (buildMazeMap s)]
                set sw [virtualSize := (sz ((length s)*cellDim) ((length $ head s)*cellDim))]
                repaint sw
        
        solveIt sw mazemap
          = do mz <- get mazemap value
               case mz of
                    Nothing -> return ()
                    Just m  -> do
                                set mazemap [value := Just (solveMaze m)]
                                repaint sw

        randMaze sw mazemap ht wt
          = do
                set ht [value := (defPuzHeight+1)]
                set wt [value := (defPuzWidth+1)]
                mzs <- sample' $ genRandomMaze defPuzHeight defPuzWidth
                set mazemap [value := Just (head mzs)]
                repaint sw
          
        
        onPaint mazemap ht wt dc _
          = do mz <- get mazemap value
               case mz of
                    Nothing -> return ()
                    Just m  -> paintMaze m ht wt dc

        paintMaze mazemap ht wt dc
          = do
                height <- get ht value
                width <- get wt value
                paintRows mazemap (height-1) (width-1) dc

        paintRows mazemap 0      width dc = paintRow mazemap 0 width dc
        paintRows mazemap height width dc
          = do
                paintRows mazemap (height-1) width dc
                paintRow  mazemap height width dc

        paintRow mazemap rrow 0 dc = paintCell mazemap rrow 0 dc
        paintRow mazemap rrow col dc
          = do
                paintRow mazemap rrow (col-1) dc
                paintCell mazemap rrow (col-1) dc

        paintCell mazemap rrow col dc = case (findWithDefault E (rrow,col) mazemap) of
           (N _ Space False _) -> 
              drawRect dc (Rect (col*cellDim) (rrow*cellDim) cellDim cellDim) [color := grey, penWidth :~ (+1), penKind := PenSolid, brushKind := BrushSolid, brushColor := grey]
           (N _ Space True _)  -> 
              drawRect dc (Rect (col*cellDim) (rrow*cellDim) cellDim cellDim) [color := yellow, penWidth :~ (+1), penKind := PenSolid, brushKind := BrushSolid, brushColor := yellow]
           (N _ Start _ _)     -> 
              drawRect dc (Rect (col*cellDim) (rrow*cellDim) cellDim cellDim) [color := green, penWidth :~ (+1), penKind := PenSolid, brushKind := BrushSolid, brushColor := green]
           (N _ End _ _)       -> 
              drawRect dc (Rect (col*cellDim) (rrow*cellDim) cellDim cellDim) [color := red, penWidth :~ (+1), penKind := PenSolid, brushKind := BrushSolid, brushColor := red]
           _                   -> 
              drawRect dc (Rect (col*cellDim) (rrow*cellDim) cellDim cellDim) [color := black, penWidth :~ (+1), penKind := PenSolid, brushKind := BrushSolid, brushColor := black]
               

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

readLines :: String -> IO [String]
readLines filePath =   do
                       s <- readFile filePath
                       return $ lines s
                       
-- Arbitrary MazeMap Generation
instance Arbitrary MazeMap where
  arbitrary = do 
    height <- genDimension
    width  <- genDimension
    genRandomMaze height width

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
