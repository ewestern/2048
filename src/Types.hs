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
} deriving (Show, Eq)

makeLenses ''Position

data Tile = Tile {  
  _tid :: Int,
  _value :: Value
} deriving (Show) 

instance Eq Tile where
  (==) (Tile i1 _) (Tile i2 _) = i1 == i2 


type Grid = M.Map Position (Maybe Tile)

type GridBehavior (Grid, M.Map Int (Behavior Tile))
type TileList = [[Maybe Tile]]

data Progress = InProgress | Lose | Win deriving (Eq, Show)

instance Show Element where
	show e = "Element"

data GameState = GameState {
  _grid :: Grid,
  _score :: Int,
  _progress :: Progress,
  _gridElement :: Element
} deriving (Show)

instance Eq GameState where
  (==) (GameState g1 s1 p1 el1) (GameState g2 s2 p2 el2) = p1 == p2 && s1 == s2 && g1 == g2


makeLenses ''GameState
