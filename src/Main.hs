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

main = do
  (evt, pushEvent) <- sync newEvent
  let handler e = sync . pushEvent =<< handleKeydown e
  J.addWindowListener "keydown" handler
  gen <- getStdGen
  cont <- J.getElementById "game-container"
  gameEl <- renderGame evt gen cont
  return ()

  