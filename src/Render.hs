{-# LANGUAGE OverloadedStrings #-}

module Render where

import Types
import Game
--import qualified JavaScript.JQuery as J
import qualified JavaScript as J
import FRP.Sodium
import System.Random
import Control.Lens
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Dequeue



uupdateGameState :: Direction -> GameState -> GameState
updateGameState d = (set grid newGrid) . (over score (+sScore)) 
  where
    (newGrid, sScore) = slideGrid d $ g ^. grid 


makeGrid :: Element -> StdGen -> Event Direction -> IO (Behavior Grid)
makeGrid par gen ed = do
  initialGrid <- (addNewTile gen) . (addNewTile gen) $ emptyGrid 
-- need way to perform arbitrary number of IO actions here
  bg <- sync $ collect updateGrid =<< hold initialGrid evt
  return bg
-- makeTile should produce a simple behavior that changes the css (And possibly removes the element from the dom) when the behavior changes.

gridToTransform :: Grid -> M.Map Int (Position, Value) 
gridToTransform  = M.foldlWithKey insert' M.empty 
	where
		insert' p (Just (Tile tid v)) acc = M.insert tid (p, v) acc
		insert' p Nothing acc = acc

updateGrid :: StdGen -> Direction -> Grid -> (Grid, Grid)
updateGrid gen d g = duplicate .  
	where
		duplicate v = (v, v)

renderGrid :: Grid -> IO ()
rederGrid g = do
-- create 16 dom elements that are removed and added as needed. 
	
  

addNewTile :: Grid -> StdGen -> Grid
addNewTile g s = let t = Tile  
  where
		empties = keys $ filter ((==) Nothing) g

updateGame :: GameState -> IO ()
updateGame gs = do
  print $ gs
  mapM_ updateTile $ concat $ _grid gs  
  return ()



 -- animation requires
makeTile :: Element -> Behavior (Maybe TileVal) -> IO (Behavior Tile)
makeTile p v parent = do
  newEl <- J.createElementWithClass "div" "tile"
  inner <- J.createElementWithClass "div" "tile-inner"
  J.appendChild newEl inner
  J.appendChild par newEl
  tile <- sync (hold 
  {-let updateCss pos val = -}


-- removes all previous classes
setCSSClass :: (Position, Tile) -> IO ()
setCSSClass (pos, tile) = 
    let klasses = ["tile", "tile-" ++ show (tileValue tile), "tile-position-" ++ positionToString pos] 
    in J.setAttribute (fromJust $ _tileElement tile) "class" (T.pack (intercalate " " klasses))  

positionToString :: Position -> String
positionToString pos = (show $ _y pos + 1) ++ "-" ++ (show $ _x pos + 1)


--hold :: a -> Event a -> Reactive (Behavior a)
renderGame :: Event Direction -> StdGen ->  Element -> IO Element
renderGame evt gen par = do
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par
  addTile <- addNewTile gridEl
  let initGame = newGame gen addTile gridEl
  bhv <- sync $ hold initGame $ fmap (stepper gen addTile initGame) evt
  kill <- sync $ listen (value bhv) updateGame
  return gridEl


addNewTile :: Element -> IO (Tile -> Tile)
addNewTile par = do
  newEl <- J.createElementWithClass "div" "tile"
  inner <- J.createElementWithClass "div" "tile-inner"
  J.appendChild newEl inner
  J.appendChild par newEl
  return $ setTileElement newEl

setTileElement :: Element -> Tile -> Tile
setTileElement el t = case t of
  Tile v p e -> Tile v p (Just el)
  Empty -> Empty

-- update the classes of tiles that have elements
-- give elements (and append) to those that do not

updateTile :: Tile -> IO ()
updateTile t = case t of
  Empty -> return ()
  Tile v p (Just el) -> addCSSClass (fromJust p, t)

putRandomTile :: StdGen -> (Tile -> Tile) -> GameState -> GameState
putRandomTile gen nd gs = case mPos of
  Nothing -> gs
  Just pos -> over grid (setTile pos $ nd $ Tile val (Just pos) Nothing) gs
  where
    [p, v] = take 2 $ randoms gen
    val =  if v < 0.9 then 2 else 4
    mPos = newTilePosition p $ gs ^. grid

newGame :: StdGen -> (Tile -> Tile) -> Element -> GameState
newGame gen nd el =  (putRandomTile gen nd) . (putRandomTile gen nd) $ GameState emptyGrid 0 InProgress el


stepper ::  StdGen -> (Tile -> Tile) -> GameState -> Direction -> GameState
stepper gen at gs dir = let setState = setOutcome gs in 
                          if setState ^. progress /= InProgress then gs
                          else (putRandomTile gen at) (updateGameState dir setState)
