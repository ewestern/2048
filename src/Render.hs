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

-- dependicies
-- Behavior Grid -> Behavior Tile -> Behavior Tiles 

makeGrid par = do
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par

data TileVal = {
  tid :: Int, 
  position :: Position,
  value :: Value,
  active:: Bool
}


collect :: (a -> s -> (b,s))-> s -> Behavior a -> Reactive (Behavior b)
collectE :: (a -> s -> (b,s))-> s -> Event a -> Reactive (Event b)

collectE :: (Dir -> Grid -> (Grid , Grid))-> Grid -> Event Dir -> Reactive (Event Grid) 


startGame evt = do
  let update' d = (\v -> (v, v)) . (updateGameState d)
  initialGrid <- makeInitialGrid
  eg <- sync $ collectE update' initialGrid evt
  listen eg updateGame


updateGameState :: Direction -> GameState -> GameState
updateGameState d = (set grid newGrid) . (over score (+sScore)) 
  where
    (newGrid, sScore) = slideGrid d $ g ^. grid 


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
