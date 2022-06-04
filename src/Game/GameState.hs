{-# LANGUAGE TemplateHaskell #-}

module Game.GameState where

import Control.Lens
import qualified Data.Text as T
import Game.Character.Player
import Game.Character.Monster

data GameState = GameState
  { _player :: Player,
    _monster :: Monster
  }
makeLenses ''GameState
