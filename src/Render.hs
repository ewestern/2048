{-# LANGUAGE OverloadedStrings #-}

module Render where

import Types
import Game
import qualified JavaScript.JQuery as J
import FRP.Sodium
import System.Random
import Control.Lens
import Data.List
import Data.Maybe
import qualified Data.Text as T


renderGame :: Event Direction -> StdGen ->  IO Element
renderGame evt gen = do
  gridEl <- J.select $ "div"
  --bhv <- sync $ accum (newGame gen gridEl) (fmap (stepper gen) evt)
  kill <- sync $ listen evt $ updateGame gridEl
  return gridEl

--initNewGame :: Element -> StdGen -> GameState -> GameState
--initNewGame el gen gs = do
--  let tp = tilePositions gs ^. grid

-- update the classes of tiles that have elements
-- give elements (and append) to those that do not
--updateGame :: Element -> GameState -> IO ()
--updateGame el gs = do
--  concat $ gs ^. grid 


updateTile :: Element -> (Position, Tile) -> IO (Tile)
updateTile par (p, t) = case t of
  Empty -> return t
  Tile v (Just el) -> addCSSClass (p, t)
  Tile v (Nothing) -> do
    newEl <- J.select $ "div"
    J.appendJQuery newEl par
    addCSSClass (p, t)

addCSSClass :: (Position, Tile) -> IO (Tile)
addCSSClass (pos, tile) = do
  let klasses = ["tile" ++ show (_value tile), "position" ++ positionToString pos]
  el <- J.addClass (T.pack (intercalate " " klasses)) $ fromJust $ _tileElement tile
  return $ Tile (_value tile) (Just el)

positionToString :: Position -> String
positionToString pos = show ((pos ^. y) * gridSize + (pos ^. x))

updateGameState :: Direction -> GameState -> IO GameState
updateGameState d g = do 
  upd <- updateGrid (g ^. gridElement) newGrid
  return $ (set grid upd) . (over score (+sScore)) $ g
  where
    (newGrid, sScore) = slideGrid d $ g ^. grid 

updateGrid :: Element -> Grid -> IO Grid
updateGrid par g =  mapM (\(y, r) -> mapM (\(x, t) -> updateTile par (Position x y, t)) $ zip [0..] r) $ zip [0..] g
--updateTile par

putRandomTile :: StdGen -> GameState -> IO GameState
putRandomTile gen gs = case mPos of
  Nothing -> return gs
  Just pos -> do
    nt <- updateTile (gs ^. gridElement) (pos, Tile (floor v) Nothing) 
    return $ over grid (setTile pos $ nt) gs
  where
    [p, v] = take 2 $ randoms gen
    mPos = newTilePosition p $ gs ^. grid

newGame :: StdGen -> Element -> IO GameState
newGame gen el =  putRandomTile gen (GameState emptyGrid 0 InProgress el) >>= (putRandomTile gen)

stepper ::  StdGen -> Direction -> GameState -> IO GameState
stepper gen dir gs = if setState ^. progress /= InProgress 
                      then return gs
                      else mkStep
  where
    mkStep = do
      upd <- updateGameState dir gs
      tileAdded <- putRandomTile gen upd
      return tileAdded
    setState = setOutcome gs



--render :: GameState -> Element
--render gs = case gs ^. progress of
--  InProgress -> showGrid
--  Lose -> applyOverlay displayGameOverOverlay $ showGrid
--  Win ->  applyOverlay displayWonOverlay $ showGrid
--  where
--    showGrid = displayGrid gs ^. grid

