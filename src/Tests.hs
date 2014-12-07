module Tests where
import Prelude hiding (Left, Right)
import Types
import Data.List
import Game
import qualified Data.Map as M
import System.Random
import Control.Applicative

putTile :: Int -> Int -> Int -> Grid -> Grid
putTile x y v = M.insert (Position x y) (Just (Tile 1 v) )

testGrid1 :: Grid
testGrid1 = (putTile 1 2 2)
            . (putTile 1 3 2)
            . (putTile 1 4 2)
            $ emptyGrid 


testGrid1_1 :: Grid
testGrid1_1 = (putTile 4 2 2)
            . (putTile 4 3 2)
            . (putTile 4 4 2)
            $ emptyGrid 



testGrid2 = (putTile 2 1 2)
            . (putTile 4 1 4)
            . (putTile 4 2 2) 
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


printSlide :: Direction -> Grid ->  IO Grid
printSlide d g = do
  let newGrid = doSlide d g
  putStrLn $ prettyPrint newGrid
  return newGrid

printSlides :: Grid -> IO ()
printSlides g = mapM_ (\d -> printSlide d g) [Up, Down, Left, Right]

showUpdatedGrid :: Direction -> ([Float], Grid) -> IO ([Float], Grid)
showUpdatedGrid d s@((x:y:xs), g) = do
  let (_, (fs, newGrid)) = updateGrid d s 
  print d
  putStrLn $ prettyPrint newGrid
  return (fs, newGrid)
  
showRotated :: Grid -> IO ()
showRotated g = do
  let gs = map (\d -> tileListToGrid $ rotator d $ gridToTileList g) [Up, Down, Left, Right]
  mapM_ (putStrLn . prettyPrint) $ gs 
  return () 

testRotator :: Direction -> TileList -> Bool 
testRotator d g = g == ((rotator d) . (unrotator d) $  g)

testRow1 :: [Maybe Tile]
testRow1 = [Just (Tile 1 2), Just (Tile 2 4), Just (Tile 1 2), Nothing]

main = do
  {-print $ map (\d -> testRotator d $ gridToTileList testGrid2) [Up, Down, Left, Right] -}
  {-print $ slideRow testRow1 -}
  {-putStrLn $ prettyPrint testGrid2-}
  {-showRotated $ testGrid2-}
  
  {-putStrLn $ prettyPrint $ testGrid2-}
  {-putStrLn $ prettyPrint . tileListToGrid . fst . (slideGrid Down) . gridToTileList $ testGrid2  -}
  {-putStrLn $ prettyPrint . tileListToGrid . fst . (slideGrid Up) . gridToTileList $ testGrid2  -}
  
  (p1:v1:p2:v2:fs) <- randoms <$> getStdGen
  let initialGrid = (putRandomTile p1 v1 1) . (putRandomTile p2 v2 2) $ emptyGrid 
  putStrLn $ prettyPrint initialGrid
  (showUpdatedGrid Up) =<< (showUpdatedGrid Down) =<< (showUpdatedGrid Right) =<< (showUpdatedGrid Left) (fs, initialGrid)
  return ()
  
