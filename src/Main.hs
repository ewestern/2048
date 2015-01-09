{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Types
import Render 
import Game
import Control.Applicative
import Control.Monad
import Prelude hiding (Left, Right)
import System.Random
import FRP.Sodium
import qualified JavaScript as J
import qualified Data.Text as T
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


makeGridBackground :: IO J.Element
makeGridBackground = do
  gc <- J.createElementWithClass "div" "grid-container"
  grs <- replicateM 4 (J.createElementWithClass "div" "grid-row" >>= J.appendChild gc)
  forM_ grs (\e -> replicateM 4 (J.createElementWithClass "div" "grid-cell" >>= J.appendChild e))
  return gc

-- Creates the Gamestate behavior
makeGrid :: J.Element -> Behavior Grid -> IO ()
makeGrid par bg = do
  gridBg <- makeGridBackground >>= J.appendChild par 
  gridEl <- J.createElementWithClass "div" "tile-container" >>= J.appendChild par 
  un <- sync $ listen (value  bg) $ renderGrid gridEl
  return () 

makeHeader :: J.Element -> Behavior Int -> IO () 
makeHeader par bi = do
  J.createChildWithClass par "h1" "title" >>= (\e -> J.innerHTML e "2048")
  scores <- J.createChildWithClass par "div" "scores-container" 
  score <- J.createChildWithClass scores "div" "score-container" 
  J.innerHTML score "0"
  sync $ listen (value $ bi) (\i -> J.innerHTML score $ T.pack $ show i)
  return () 
 
 
main = do
  (evt, pushEvent) <- sync newEvent
  let handler e = sync . pushEvent =<< handleKeydown e
  J.addWindowListener "keydown" handler
  (body:_) <- J.getElementsByTagName "body"
  cont <- J.createElementWithClass "div" "container" >>= J.appendChild body
  heading <- J.createElementWithClass "div" "heading" >>= J.appendChild cont
  gameCont <- J.createElementWithClass "div" "game-container" >>= J.appendChild cont 

  (p1:v1:p2:v2:fs) <- randoms <$> getStdGen
  let initialGrid = (putRandomTile p1 v1 1) . (putRandomTile p2 v2 2) $ emptyGrid  
  let initialGS = GameState initialGrid 0 InProgress fs
  bgs <- sync $ collect updateGameState initialGS =<< hold Nope evt 
  makeGrid gameCont (_grid <$> bgs) 
  makeHeader heading (_score <$> bgs)
  return ()

  
