module Tests where
import Types
import Data.List
import Game
import qualified Data.Map as M

putTile :: Int -> Int -> Grid -> Grid
putTile x y = M.insert (Position x y) (Just (Tile 1 2) )

testGrid1 :: Grid
testGrid1 = (putTile 1 2)
            . (putTile 1 3)
            . (putTile 1 4)
            $ emptyGrid 


testGrid1_1 :: Grid
testGrid1_1 = (putTile 4 2)
            . (putTile 4 3)
            . (putTile 4 4)
            $ emptyGrid 


testConversion :: Grid -> Bool
testConversion g = g == (tileListToGrid . gridToTileList $ g )

testSlide :: Direction -> Grid -> Grid -> Bool
testSlide d g check = check == (doSlide d g) 

doRotate :: Grid -> Grid
doRotate = tileListToGrid . reverse . transpose . gridToTileList
--doRotate = tileListToGrid . rotate . gridToTileList

doSlide :: Direction -> Grid -> Grid
doSlide d = tileListToGrid . fst . (slideGrid d) . gridToTileList 


printSlide :: Direction -> Grid -> String
printSlide d g = prettyPrint $ doSlide d g


main = do
  putStrLn $ prettyPrint testGrid1
  putStrLn $ prettyPrint . (tileListToGrid . gridToTileList) $ testGrid1 
--  print $ testConversion testGrid1
--  putStrLn $ prettyPrint $ doRotate testGrid1
--  putStrLn $ printSlide Nope testGrid1
--  putStrLn $ printSlide Down testGrid1

