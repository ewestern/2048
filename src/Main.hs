{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Types
import Render (renderGame)
--import Control.Monad
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
  n <- getKey ev
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
  J.append game body
  let handler e = sync . pushEvent =<< handleKeydown e
  J.on handler "keydown" handlerSettings body
  return ()
  --grid <- makeGridContainer
  --cellContainer <- makeCellContainer
  --(gBeh, gPush) <- newBehavior Nope
  -- get the value of body size (which may change over time)
  --startSize <- sync (sample bodySize)
  --bhv <- setEvent (T.pack "keydown") body
  --sync $ listen (values bhv) $ updateGrid
  --return ()



  