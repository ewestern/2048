{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Types
import Render (renderGame)
--import Control.Monad
import Prelude hiding (Left, Right)
import System.Random
import FRP.Sodium
import qualified JavaScript.JQuery as J
import Control.Lens
import GHCJS.Types
import GHCJS.Foreign

foreign import javascript unsafe 
  "$1.keyCode"
  getKey :: J.Event -> IO Int

handlerSettings = J.HandlerSettings False False False Nothing Nothing

handleKeydown :: J.Event -> IO (Direction)
handleKeydown ev = do
  print "event"
  n <- getKey ev
  print n
  case n of
    38 -> return Up
    40 -> return Down
    37 -> return Left
    39 -> return Right

main = do
  body <- J.select "body"
  (evt, pushEvent) <- sync newEvent
  gen <- getStdGen
  gameEl <- renderGame evt gen
  J.appendJQuery gameEl body
  let handler e = sync . pushEvent =<< handleKeydown e
  J.on handler "keydown" handlerSettings body
  return ()

  