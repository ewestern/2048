{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Types
import Render (renderGame)
--import Control.Monad
import Prelude hiding (Left, Right)
import System.Random
import FRP.Sodium
import qualified JavaScript as J
import Control.Lens
import GHCJS.Types
import GHCJS.Foreign
import Data.Default

handleKeydown :: J.Event -> IO Direction
handleKeydown ev = do
  n <- J.getKey ev
  print n
  case n of
    38 -> return Up
    40 -> return Down
    37 -> return Left
    39 -> return Right
    _ -> return Nope


makeGrid :: Element -> Event Direction -> IO (Behavior Grid )
makeGrid par ed = do
-- create 16 dom elements that are removed and added as needed. 
-- However, adding it back is still an IO action, and we're back where we started
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par
  {-(e1:e2:es) <- replicateM (gridSize ^ 2)  createTileEl-}
  gen <- getStdGen
  initialGrid <- (addNewTile gen) . (addNewTile gen) $ emptyGrid 
  -- best way, grid remains source of truth, search for tid on dom
  -- on list
  bg <- sync $ collect updateGrid =<< hold initialGrid evt
  un <- sync $ listen renderGrid $ values bg



main = do
  (evt, pushEvent) <- sync newEvent
  let handler e = sync . pushEvent =<< handleKeydown e
  J.addWindowListener "keydown" handler
  gen <- getStdGen
  cont <- J.getElementById "game-container"
  gameEl <- renderGame evt gen cont
  return ()

  
