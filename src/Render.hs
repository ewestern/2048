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
import qualified Data.Map as M
import Control.Applicative


readText :: T.Text -> Int
readText = read . T.unpack

renderGrid :: J.Element -> Grid -> IO ()
renderGrid par g = do
  print  g
  es <- J.getElementsByClassName  "tile"   
  ids <- map readText <$> mapM J.getId es
  mapM_ updateEl es
  mapM_ createEl $ (M.keys trans) \\ ids  
	where
    trans = gridToTransform g
    updateEl el = do
      t <- J.getId el
      case M.lookup (readText t) $ gridToTransform g of
        Nothing -> J.removeChild par el
        Just (p, v) -> setCSSClass el (p, v) $ readText t 
    createEl i = do
      el <- createTileEl
      J.appendChild par el	
      setCSSClass el (fromJust $ M.lookup i trans)  i


createTileEl :: IO J.Element 
createTileEl = do
  newEl <- J.createElementWithClass "div" "tile"
  inner <- J.createElementWithClass "div" "tile-inner"
  J.appendChild newEl inner
  return newEl


setCSSClass :: J.Element -> (Position, Value)-> Int -> IO ()
setCSSClass el (pos, v) i =   
    let klasses = ["tile", "tile-" ++ show v, "tile-position-" ++ positionToString pos]
        set' = J.setAttribute el 
    in set' "class" (T.pack (intercalate " " klasses)) >>  set'  "id"  (T.pack . show $ i)

positionToString :: Position -> String
positionToString pos = (show $ _y pos)  ++ "-" ++ (show $ _x pos)






