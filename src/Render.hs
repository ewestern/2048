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
import Data.List ((\\))





renderGrid :: Element -> Grid -> IO ()
renderGrid par g = do
  es <- getElementsByClassName  "tile"   
  let trans = gridToTransform g 
  mapM_ updateEl es
  mapM_ createEL $ (M.getKeys trans) \\ map read es
  where
    updateEl el = do
      t <- getId el
      case M.lookup (read t) $ gridToTransform g of
        Nothing -> removeChild par el
        Just (p, v) -> setCSSClass el (p, v) $ read t 
    createEl i = do
      el <- createTileEl
      setCSSClass el (fromJust $ M.lookup i trans) $ read i


createTileEl :: IO Element 
createTileEl = do
  newEl <- J.createElementWithClass "div" "tile"
  inner <- J.createElementWithClass "div" "tile-inner"
  J.appendChild newEl inner
  return newEl

addNewTile :: StdGen -> Grid -> Grid
addNewTile s g = let t = Tile  
  where
		empties = keys $ filter ((==) Nothing) g


 ---
 --
updateGameState :: Direction -> GameState -> GameState
updateGameState d = (set grid newGrid) . (over score (+sScore)) 
  where
    (newGrid, sScore) = slideGrid d $ g ^. grid 


makeGrid :: Element -> StdGen -> Event Direction -> IO (Behavior Grid)
makeGrid par gen ed = do
  initialGrid <- (addNewTile gen) . (addNewTile gen) $ emptyGrid 
  bg <- sync $ collect updateGrid =<< hold initialGrid evt
  return bg
-- makeTile should produce a simple behavior that changes the css (And possibly removes the element from the dom) when the behavior changes.


updateGrid :: StdGen -> Direction -> Grid -> (Grid, Grid)
updateGrid gen d g = duplicate .  
	where
		duplicate v = (v, v)


updateGame :: GameState -> IO ()
updateGame gs = do
  print $ gs
  mapM_ updateTile $ concat $ _grid gs  
  return ()


-- removes all previous classes
setCSSClass :: Element -> (Position, Value)-> ID -> IO ()
setCSSClass el (pos, v) i = do  
    let klasses = ["tile", "tile-" ++ show , "tile-position-" ++ positionToString pos] 
    J.setAttribute el "class" (T.pack (intercalate " " klasses))  
    J.setAttribute e  "id"  (show i)

positionToString :: Position -> String
positionToString pos = (show $ _y pos)  ++ "-" ++ (show $ _x pos)


--hold :: a -> Event a -> Reactive (Behavior a)
renderGame :: Event Direction -> StdGen ->  Element -> IO Element
renderGame evt gen par = do
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par
  addTile <- addNewTile gridEl
  let initGame = newGame gen addTile gridEl
  bhv <- sync $ hold initGame $ fmap (stepper gen addTile initGame) evt
  kill <- sync $ listen (value bhv) updateGame
  return gridEl


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
