  module Game where

import Types
import Prelude hiding (Empty, Right, Left)
import Control.Lens
import Data.List
import Data.Maybe
import System.Random
import qualified Data.Map as M

gridSize = 4
winningVal = 2048
tileProbability = 0.9
initialTiles = 2

gridToTransform :: Grid -> M.Map Int (Position, Value) 
gridToTransform  = M.foldlWithKey insert' M.empty 
	where
		insert' p (Just (Tile tid v)) acc = M.insert tid (p, v) acc
		insert' p Nothing acc = acc


gridToTileList :: Grid -> TileList
gridToTileList g = [[fromJust $ M.lookup (Position x y) g | y <- [1..gridSize]] | x <- [1..gridSize]]

rotate :: TileList -> TileList 
rotate = map reverse . transpose

emptyGrid :: Grid
emptyGrid = M.fromList [(Position x y, Nothing) | x <- [1..gridSize], y <- [1..gridSize]]

-- creates a new row, slid to the left, with appropriate values merged if necessary
--HERE
slideRow :: [Maybe Tile] -> ([Maybe Tile], Int)
slideRow row = (take gridSize (newRow ++ (repeat Nothing)), score)
  where 
    grouped = group $ filter (\t -> t /= Nothing) row 
    newRow = map (\ls -> Tile (_tid . head $ ls) (sum $ map tileValue ls)) grouped
    score = sum . (map tileValue) $ concat $ filter (\ls -> length ls > 1) grouped

slideGrid :: Direction -> TileList ->  (TileList, Int)
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

tileValue :: Maybe Tile -> Value
tileValue (Just t) = _value t
tileValue Nothing = 0

--game is won if there is a tile with value of winningVal
hasWon :: Grid -> Bool
hasWon = not . isNothing . find ((== winningVal) . tileValue) . concat

--game is lost if sliding in all directions results in the same grid
hasLost :: Grid -> Bool
hasLost g = and $ map (\d -> g == (fst $ slideGrid d g)) [Down, Right, Up, Left]

emptyPositions :: Grid -> [Position]
emptyPositions = map fst . filter ((== Empty) . snd) . tilePositions


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




