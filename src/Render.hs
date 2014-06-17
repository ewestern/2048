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


--hold :: a -> Event a -> Reactive (Behavior a)
renderGame :: Event Direction -> StdGen ->  IO Element
renderGame evt gen = do
  gridEl <- J.select $ "<div class=\"grid\"></div>"
  newDiv <- addDiv gridEl
  --bhv <- sync $ accum (newGame gen newDiv gridEl) (fmap (stepper gen newDiv) evt)
  let initGame = newGame gen newDiv gridEl
  --let gchange = 
  bhv <- sync $ hold initGame $ fmap (stepper gen newDiv initGame) evt
  kill <- sync $ listen (value bhv) $ (updateGame gridEl)
  return gridEl


addDiv :: Element -> IO (Tile -> Tile)
addDiv par = do
  newEl <- J.select $ "<div />"
  J.appendJQuery newEl par
  return $ setTileElement newEl

setTileElement :: Element -> Tile -> Tile
setTileElement el t = case t of
  Tile v p e -> Tile v p (Just el)
  Empty -> Empty
--initNewGame :: Element -> StdGen -> GameState -> GameState
--initNewGame el gen gs = do
--  let tp = tilePositions gs ^. grid

-- update the classes of tiles that have elements
-- give elements (and append) to those that do not
updateGame :: Element -> GameState -> IO ()
updateGame el gs = do
  print (gs ^. grid )
  mapM (updateTile el) $ concat $ gs ^. grid 
  return ()

--createElement :: IO Element


updateTile :: Element -> Tile -> IO ()
updateTile par t = case t of
  Empty -> return ()
  Tile v p (Just el) -> addCSSClass (fromJust p, t)
  --Tile v p (Nothing) -> do
  --  newEl <- J.select $ "div"
  --  J.appendJQuery newEl par
  --  addCSSClass (fromJust p, t)

addCSSClass :: (Position, Tile) -> IO ()
addCSSClass (pos, tile) = do
  let klasses = ["tile" ++ show (tileValue tile), "position" ++ positionToString pos]
  J.setAttr "class" (T.pack (intercalate " " klasses)) $ fromJust $ _tileElement tile
  return $ ()

positionToString :: Position -> String
positionToString pos = show ((pos ^. y) * gridSize + (pos ^. x))




updateGameState :: Direction -> GameState -> GameState
updateGameState d g = (set grid newGrid) . (over score (+sScore)) $ g
  where
    (newGrid, sScore) = slideGrid d $ g ^. grid 

--updateGameState :: Direction -> GameState -> IO GameState
--updateGameState d g = do 
--  upd <- updateGrid (g ^. gridElement) newGrid
--  return $ (set grid upd) . (over score (+sScore)) $ g
--  where
--    (newGrid, sScore) = slideGrid d $ g ^. grid 

--updateGrid :: Element -> Grid -> IO Grid
--updateGrid par g =  mapM (\(y, r) -> mapM (\(x, t) -> updateTile par (Position x y, t)) $ zip [0..] r) $ zip [0..] g

putRandomTile :: StdGen -> (Tile -> Tile) -> GameState -> GameState
putRandomTile gen nd gs = case mPos of
  Nothing -> gs
  Just pos -> over grid (setTile pos $ nd $ Tile (floor v) (Just pos) Nothing) gs
  where
    [p, v] = take 2 $ randoms gen
    mPos = newTilePosition p $ gs ^. grid

--putRandomTile :: StdGen -> GameState -> IO GameState
--putRandomTile gen gs = case mPos of
--  Nothing -> return gs
--  Just pos -> do
--    nt <- updateTile (gs ^. gridElement) (pos, Tile (floor v) Nothing) 
--    return $ over grid (setTile pos $ nt) gs
--  where
--    [p, v] = take 2 $ randoms gen
--    mPos = newTilePosition p $ gs ^. grid

newGame :: StdGen -> (Tile -> Tile) -> Element -> GameState
newGame gen nd el =  (putRandomTile gen nd) . (putRandomTile gen nd) $ GameState emptyGrid 0 InProgress el
--newGame :: StdGen -> Element -> IO GameState
--newGame gen el =  putRandomTile gen (GameState emptyGrid 0 InProgress el) >>= (putRandomTile gen)


stepper ::  StdGen -> (Tile -> Tile) -> GameState -> Direction -> GameState
stepper gen nd gs dir = if setState ^. progress /= InProgress 
                      then gs
                      else (putRandomTile gen nd) (updateGameState dir setState)
  where
    setState = setOutcome gs

--stepper ::  StdGen -> Direction -> GameState -> IO GameState
--stepper gen dir gs = if setState ^. progress /= InProgress 
--                      then return gs
--                      else mkStep
--  where
--    mkStep = do
--      upd <- updateGameState dir gs
--      tileAdded <- putRandomTile gen upd
--      return tileAdded
--    setState = setOutcome gs



--render :: GameState -> Element
--render gs = case gs ^. progress of
--  InProgress -> showGrid
--  Lose -> applyOverlay displayGameOverOverlay $ showGrid
--  Win ->  applyOverlay displayWonOverlay $ showGrid
--  where
--    showGrid = displayGrid gs ^. grid

