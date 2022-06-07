{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Character.Monster (
  Monster (..),
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Text as T
import Game.Character.Character

data Monster = Monster
  { _monsterHp :: Int,
    _monsterName :: T.Text
  }
  deriving (Show)

makeLenses ''Monster

instance Character Monster where
  hp = monsterHp
  name = monsterName
  addHp = addHp'
