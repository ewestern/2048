{-# LANGUAGE OverloadedStrings, TemplateHaskell, Rank2Types, FlexibleInstances #-}
module Types where


import Control.Lens hiding (element)
import FRP.Sodium
import qualified Data.Set as S
import qualified Data.Map as M
import qualified JavaScript as J

data Direction = Left | Right | Up | Down | Nope deriving(Eq, Show)

type Value = Int

data Position = Position {
  _x :: Int,
  _y :: Int
} deriving (Show, Eq, Ord)

--makeLenses ''Position

data Tile = Tile {  
  _tid :: Int,
  _value :: Value
} deriving (Show) 

instance Eq Tile where
  (==) (Tile i1 _) (Tile i2 _) = i1 == i2 


type Grid = M.Map Position (Maybe Tile)

type GridBehavior = (Grid, M.Map Int (Behavior Tile))

type TileList = [[Maybe Tile]]

data Progress = InProgress | Lose | Win deriving (Eq, Show)

data GameState = GameState {
  _grid :: Grid,
  _score :: Int,
  _progress :: Progress,
  _rs :: [Float]
} deriving (Show, Eq)

makeLenses ''GameState
