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

putRandomTile :: Float -> Float -> Int ->  Grid -> Grid
putRandomTile p v i g = 
  let newPos = newTilePosition p g
      newVal = if v < 0.9 then 2 else 4
  in case newPos of
    Nothing -> g
    Just pos -> M.insert pos (Just $ Tile i newVal) g

newTilePosition :: Float -> Grid -> Maybe Position
newTilePosition x g = case emptyPositions g of
  [] -> Nothing
  es -> randomChoice x es 


randomChoice :: Float -> [b] -> Maybe b
randomChoice f l = 
  let idx = floor $ f * fromIntegral (length l)
  in if length l == 0 then Nothing else Just $ l !! (floor  f * length l)


gridToTransform :: Grid -> M.Map Int (Position, Value) 
gridToTransform  = M.foldlWithKey insert' M.empty 
	where
		insert' acc p (Just (Tile tid v)) = M.insert tid (p, v) acc
		insert' acc p Nothing = acc


{-updateGameState :: Direction -> GameState -> GameState-}
{-updateGameState d = (set grid newGrid) . (over score (+sScore)) -}
  {-where-}
    {-(newGrid, sScore) = slideGrid d $ g ^. grid -}

updateGrid :: StdGen -> Direction -> Grid -> ((Int, Grid), Grid)
updateGrid gen d g = 
  let (tl, i) =  slideGrid d $ gridToTileList g 
      newGrid = putRandomTile p v newId $ tileListToGrid tl
  in ((i, newGrid), newGrid) 
  where
    count' a Nothing = a
    count' a (Just (Tile i v)) = if i > a then i else a 
    newId = (+1) $ M.foldl count' 1 g
    (p:v:_) = take 2 $ randoms gen
    duplicate v = (v, v)

gridToTileList :: Grid -> TileList
gridToTileList g = [[fromJust $ M.lookup (Position x y) g | y <- [1..gridSize]] | x <- [1..gridSize]]

tileListToGrid :: TileList -> Grid
tileListToGrid tl = M.fromList . concat $ map (\(r, y) -> map (\(mt, x) -> (Position x y, mt)) (zip r [1..])) (zip tl [1..])

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
    newRow = map mergeTiles grouped
    score = sum . (map tileValue) $ concat $ filter (\ls -> length ls > 1) grouped
    mergeTiles ls@(Just (Tile i v):ts) = Just $ Tile i (sum $ map tileValue ls)

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
hasWon :: TileList -> Bool
hasWon = not . isNothing . find ((== winningVal) . tileValue) . concat

--game is lost if sliding in all directions results in the same grid
hasLost :: TileList -> Bool
hasLost g = and $ map (\d -> g == (fst $ slideGrid d g)) [Down, Right, Up, Left]

emptyPositions :: Grid -> [Position]
emptyPositions = M.keys .  M.filter  (\m -> m == Nothing) 
{-map fst . filter ((== Empty) . snd) . tilePositions-}


setOutcome :: GameState -> GameState
setOutcome gs
  | hasWon . gridToTileList . _grid $ gs = set progress Win gs
  | hasLost . gridToTileList . _grid $ gs = set progress Lose gs
  | otherwise = gs




