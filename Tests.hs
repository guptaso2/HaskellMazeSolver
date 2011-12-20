-- Tests.hs
-- by Sonny Gupta guptason (and  Kaushik Ganguly kganguly)

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XTypeSynonymInstances #-}

module Tests where
import Maze
import Test.HUnit hiding (Node)
import Data.HashMap as M

runTests :: IO ()
runTests = do _ <- runTestTT (TestList [ t1, t2, t3, t4, t5 ])
              return ()

{--                       
t0 :: Test
t0 = 
  out <- readLines "test.txt"
  succeed out
  where
    succeed ["*****S*","***** *","***** *","***   *"
          ,"*** ***","E   ***","*** ***"] = assert True
    succeed _                             = assert False
--}

t1 :: Test
t1 = showMaze testMap 7 7 ~?= testMazeStr

t2 :: Test
t2 = showRows testMap 6 6 ~?= testMazeStr

t3 :: Test
t3 = TestList [ showRow testMap 0 6 ~?= "*****S*",
                showRow testMap 1 6 ~?= "***** *",
                showRow testMap 2 6 ~?= "***** *",
                showRow testMap 3 6 ~?= "***   *",
                showRow testMap 4 6 ~?= "*** ***",
                showRow testMap 5 6 ~?= "E   ***",
                showRow testMap 6 6 ~?= "*** ***" ]

t4 :: Test
t4 = TestList [ showCell testMap 0 0 ~?= "*",
                showCell testMap 0 5 ~?= "S",
                showCell testMap 0 6 ~?= "*",
                showCell testMap 3 3 ~?= " ",
                showCell testMap 5 0 ~?= "E",
                showCell testMap 6 0 ~?= "*",
                showCell testMap 6 3 ~?= " ",                
                showCell testMap 6 6 ~?= "*" ]                
     
t5 :: Test
t5 = show (toList (buildMazeMap testRows)) ~?= show testList




testList :: [ ((Int, Int), Node) ]
testList = [((0,5),N (0,5) Start False [(1,5)]),((1,5),N (1,5) Space False [(0,5),(2,5)]),((2,5),N (2,5) Space False [(1,5),(3,5)]),((3,3),N (3,3) Space False [(4,3),(3,4)]),((3,5),N (3,5) Space False [(2,5),(3,4)]),((3,4),N (3,4) Space False [(3,3),(3,5)]),((4,3),N (4,3) Space False [(3,3),(5,3)]),((5,1),N (5,1) Space False [(5,0),(5,2)]),((5,0),N (5,0) End False [(5,1)]),((5,3),N (5,3) Space False [(4,3),(5,2),(6,3)]),((5,2),N (5,2) Space False [(5,1),(5,3)]),((6,3),N (6,3) Space False [(5,3)])]

testMap :: MazeMap
testMap = fromList testList

testMazeStr :: String
testMazeStr = "*****S*\n***** *\n***** *\n***   *\n*** ***\nE   ***\n*** ***"

testRows :: [String]
testRows = [ "*****S*",
             "***** *",
             "***** *",
             "***   *",
             "*** ***",
             "E   ***",
             "*** ***" ]