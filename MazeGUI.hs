-- Maze.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module Main where
import Solver
import RandomMaze
import Data.HashMap as M
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
