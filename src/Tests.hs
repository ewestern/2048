module Tests where
import Types
import Game
import qualified Data.Map as M

putTile :: Int -> Int -> Grid -> Grid
putTile x y = M.insert (Position x y) (Just (Tile 1 2) )

testGrid1 :: Grid
testGrid1 = (putTile 1 2)
            . (putTile 1 3)
            . (putTile 1 4)
            $ emptyGrid 

 
