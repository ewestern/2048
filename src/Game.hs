module Game where

import Types
import Prelude hiding (Empty, Right, Left)
import Control.Lens
import Data.List
import Data.Maybe
import System.Random

gridSize = 4
winningVal = 2048
tileProbability = 0.9
initialTiles = 2

rotate :: Grid -> Grid
rotate = map reverse . transpose

emptyGrid :: Grid
emptyGrid = replicate gridSize $ replicate gridSize Empty

readTile :: Position -> Grid -> Tile
readTile (Position x y) g = (g !! y) !! x 

setTile :: Position -> Tile -> Grid -> Grid
setTile (Position x y) t g = let r = take x row ++ [t] ++ drop (x + 1) row in take y g ++ [r] ++ drop (y + 1) g
  where row = g !! y

-- creates a new row, slid to the left, with appropriate values merged if necessary
--HERE
slideRow :: Row -> (Row, Int)
slideRow row = (take gridSize (newRow ++ (repeat Empty)), score)
  where 
    grouped = group $ filter (\t -> t /= Empty) row 
    newRow = map (\ls -> Tile (sum $ map _value ls) (_tileElement $ head ls)) grouped
    score = sum . (map _value) $ concat $ filter (\ls -> length ls > 1) grouped



slideGrid :: Direction -> Grid ->  (Grid, Int)
slideGrid dir g = (unrotator newRows, sum scorez)
  where 
    (newRows, scorez) = unzip $ map slideRow $ rotator g
    rotator = case dir of
      Down -> rotate
      Right -> rotate . rotate
      Up -> rotate . rotate . rotate
      _ -> id
    unrotator = case dir of
      Up -> rotate
      Right -> rotate . rotate
      Down -> rotate . rotate . rotate
      _ -> id


--game is won if there is a tile with value of winningVal
hasWon :: Grid -> Bool
hasWon = not . isNothing . find ((== winningVal) . _value) . concat

--game is lost if sliding in all directions results in the same grid
hasLost :: Grid -> Bool
hasLost g = and $ map (\d -> g == (fst $ slideGrid d g)) [Down, Right, Up, Left]

emptyPositions :: Grid -> [Position]
emptyPositions = map fst . filter ((== Empty) . snd) . tilePositions

--remakeGrid :: [(Position, Tile)] -> Grid
--remakeGrid ps = 

tilePositions :: Grid -> [(Position, Tile)]
tilePositions = concat . zipWith dZip [0..]
  where 
    dZip y ls = map (\(x, t) ->  (Position x y, t)) $ zip [0..] ls

newTilePosition :: Float -> Grid -> Maybe Position
newTilePosition x g = case empties of
  [] -> Nothing
  otherwise -> Just $ empties !! (floor $ x * (fromIntegral $ length empties))
  where
    empties = emptyPositions g  


setOutcome :: GameState -> GameState
setOutcome gs
  | hasWon $ gs ^. grid = set progress Win gs
  | hasLost $ gs ^. grid = set progress Lose gs
  | otherwise = gs

--let (x1:y1:x2:y2:ys) = randoms gen in (putRandomTile x1 y1) . (putRandomTile x2 y2)




