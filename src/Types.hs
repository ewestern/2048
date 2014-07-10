{-# LANGUAGE OverloadedStrings, TemplateHaskell, Rank2Types, FlexibleInstances #-}
module Types where


import Control.Lens hiding (element)
import FRP.Sodium
import qualified Data.Set as S
import qualified Data.Map as M
import GHCJS.Types

import qualified JavaScript as J
type Element = J.DomElement

--type Element = String

data Direction = Left | Right | Up | Down | Nope deriving(Eq)

type Value = Int
data Position = Position {
  _x :: Int,
  _y :: Int
} deriving (Show)

instance Ord Position where
  (<=) (Position x1 y1) (Position x2 y2) = x1 < x2 || (x1 == x2 && y1 <= y2)  

instance Eq Position where
  (==) (Position x1 y1) (Position x2 y2) = x1 == x2 && y1 == y2

instance Show (JSRef J.DomElement_) where
  show el = "Element"

makeLenses ''Position

data Tile = Tile {  
  _value :: Value,
  _position :: Maybe Position,
  _tileElement :: Maybe Element
} | Empty

instance Eq Tile where
  (==) Empty Empty = True
  (==) (Tile v1 p1 e1) (Tile v2 p2 e2) = v1 == v2 && p1 == p2
  (==) _ _ = False

instance Show Tile where
  show (Tile v p t) = "Tile (" ++ show v ++ ", " ++ show p ++ ")"
  show Empty = "Empty"

type Row = [Tile]
type Grid = [Row]


data Progress = InProgress | Lose | Win deriving (Eq, Show)

data GameState = GameState {
  _grid :: Grid,
  _score :: Int,
  _progress :: Progress,
  _gridElement :: Element
} deriving (Show)

instance Eq GameState where
  (==) (GameState g1 s1 p1 el1) (GameState g2 s2 p2 el2) = p1 == p2 && s1 == s2 && compare g1 g2
    where
      compare g1 g2 = and $ map (\(t1, t2) -> _value t1 == _value t2) $ zip (concat g1) (concat g2)

--instance Monad GameState where
--  (>>=)
--defaultGameState :: GameState
--defaultGameState = GameState emptyGrid 0 InProgress

makeLenses ''GameState
