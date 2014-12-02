{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Types
import Render 
import Game
import Control.Applicative
import Prelude hiding (Left, Right)
import System.Random
import FRP.Sodium
import qualified JavaScript as J
import Control.Lens
import GHCJS.Types
import GHCJS.Foreign

handleKeydown :: J.Event -> IO Direction
handleKeydown ev = do
  n <- J.getKey ev
  case n of
    38 -> return Up
    40 -> return Down
    37 -> return Left
    39 -> return Right
    _ -> return Nope

makeGrid :: J.Element -> Event Direction -> IO (Behavior (Int, Grid))
makeGrid par ed = do
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par
  (p1:v1:p2:v2:fs) <- randoms <$> getStdGen
  let initialGrid = (putRandomTile p1 v1 1) . (putRandomTile p2 v2 2) $ emptyGrid  
  bg <- sync $ collect updateGrid (fs, initialGrid) =<< hold Nope ed
  un <- sync $ listen (value $ snd <$> bg) $ renderGrid gridEl
  return bg

--stepper ::  StdGen -> (Tile -> Tile) -> GameState -> Direction -> GameState
--stepper gen at gs dir = let setState = setOutcome gs in 
--													if setState ^. progress /= InProgress then gs
--													else (putRandomTile gen at) (updateGameState dir setState)

main = do
  (evt, pushEvent) <- sync newEvent
  let handler e = sync . pushEvent =<< handleKeydown e
  J.addWindowListener "keydown" handler
  
  cont <- J.createElementWithClass "div" "game-container"
  (body:_) <- J.getElementsByTagName "body"
  J.appendChild body cont
  gameEl <- makeGrid cont evt 
  return ()

  
