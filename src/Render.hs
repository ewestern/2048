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


 ---
 --

makeGrid :: Element -> Event Direction -> IO (Behavior Grid)
makeGrid par ed = do
  initialGrid <- (addNewTile gen) . (addNewTile gen) $ emptyGrid 
  bg <- sync $ collect updateGrid =<< hold initialGrid evt
  return bg
-- makeTile should produce a simple behavior that changes the css (And possibly removes the element from the dom) when the behavior changes.


setCSSClass :: Element -> (Position, Value)-> ID -> IO ()
setCSSClass el (pos, v) i =   
    let klasses = ["tile", "tile-" ++ show , "tile-position-" ++ positionToString pos]
        set' = J.setAttribute el 
    in set' "class" (T.pack (intercalate " " klasses)) >>  set'  "id"  (show i)

positionToString :: Position -> String
positionToString pos = (show $ _y pos)  ++ "-" ++ (show $ _x pos)





stepper ::  StdGen -> (Tile -> Tile) -> GameState -> Direction -> GameState
stepper gen at gs dir = let setState = setOutcome gs in 
                          if setState ^. progress /= InProgress then gs
                          else (putRandomTile gen at) (updateGameState dir setState)
