{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Character.Player (
  Player (..),
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Text as T
import Game.Character.Character

data Player = Player
  { _playerHp :: Int,
    _playerName :: T.Text
  }
makeLenses ''Player

instance Character Player where
  characterHp = playerHp
  characterName = playerName
