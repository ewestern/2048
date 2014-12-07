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
  in if length l == 0 then Nothing else Just $ l !! idx 


sameGrid :: Direction -> Grid -> Bool 
sameGrid d g = g == (tileListToGrid . fst . (slideGrid d) . gridToTileList) g

gridToTransform :: Grid -> M.Map Int (Position, Value) 
gridToTransform  = M.foldlWithKey insert' M.empty 
	where
		insert' acc p (Just (Tile tid v)) = M.insert tid (p, v) acc
		insert' acc p Nothing = acc

-- todo: at game level, check slideGrid to see if any movement, if not, change direction to Nope
updateGrid :: Direction -> ([Float], Grid) -> ((Int, Grid), ([Float], Grid))
updateGrid Nope (fs, g) = ((0, g), (fs, g))
updateGrid d ((p:v:ys), g) = 
  let (tl, i) =  slideGrid d $ gridToTileList g 
      newGrid = putRandomTile p v newId $ tileListToGrid tl
  in ((i, newGrid), (ys, newGrid))
  where
    count' a Nothing = a
    count' a (Just (Tile i v)) = if i > a then i else a 
    newId = (+1) $ M.foldl count' 1 g


gridToTileList :: Grid -> TileList
gridToTileList g = [[fromJust $ M.lookup (Position x y) g | x <- [1..gridSize]] | y <- [1..gridSize]]

tileListToGrid :: TileList -> Grid
tileListToGrid tl = M.fromList . concat $ map (\(r, y) -> map (\(mt, x) -> (Position x y, mt)) (zip r [1..])) (zip tl [1..])

rotate :: [[a]] -> [[a]] 
rotate = map reverse . transpose

emptyGrid :: Grid
emptyGrid = M.fromList [(Position x y, Nothing) | x <- [1..gridSize], y <- [1..gridSize]]

-- creates a new row, slid to the left, with appropriate values merged if necessary
--HERE
slideRow :: [Maybe Tile] -> ([Maybe Tile], Int)
slideRow row = (take gridSize (newRow ++ (repeat Nothing)), score)
  where 
    grouped = group2 $ filter (\t -> t /= Nothing) row 
    newRow = map mergeGroup  grouped
    score = sum . (map tileValue) $ concat $ filter (\ls -> length ls > 1) grouped

mergeGroup :: [Maybe Tile] -> Maybe Tile
mergeGroup ls@(Just (Tile i _):_) = Just $ Tile i (sum $ map tileValue ls)


group2 :: [Maybe Tile] -> [[Maybe Tile]]
group2 [] = []
group2 (x:[]) = [[x]]
group2 (x:y:zs) = if tileValue x == tileValue y 
                then ([x,y]):(group2 zs) 
                else ([x]):(group2 (y:zs)) 

slideGrid :: Direction -> TileList ->  (TileList, Int)
slideGrid dir g = (unrotator dir newRows, sum scorez)
  where 
    (newRows, scorez) = unzip $ map slideRow $ rotator dir g

rotator :: Direction -> [[a]] -> [[a]] 
rotator dir = case dir of
        Down -> rotate
        Right -> rotate . rotate
        Up -> rotate . rotate . rotate
        _ -> id

unrotator :: Direction -> [[a]] -> [[a]] 
unrotator dir = case dir of
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

--game is lost if all spots are taken 
hasLost :: TileList -> Bool
hasLost = all (all (\mt -> mt /= Nothing)) 

{-hasLost g = and $ map (\d -> g == (fst $ slideGrid d g)) [Down, Right, Up, Left]-}

emptyPositions :: Grid -> [Position]
emptyPositions = M.keys .  M.filter  (\m -> m == Nothing) 
{-map fst . filter ((== Empty) . snd) . tilePositions-}





prettyPrint :: Grid -> String
prettyPrint g = concat $ map showRow $ zip (gridToTileList g)  [1..]
	where
		showRow (ls, y) = (replicate 8 '_') ++ "\n" ++  (concat $ map showTile ( zip ls [1..] )) ++ "\n"
			where
				showTile (t,x)
					| x == 1 = "|" ++ show (tileValue t) ++ "|"
					| otherwise = show (tileValue t) ++ "|" 



