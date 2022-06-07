{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Character.Player
  ( PlayerTrait (..),
    Player (..),
    -- custom lenses
    traits,
  )
where

import Control.Lens
import Control.Monad.State
import qualified Data.Text as T
import Game.Character.Character

data PlayerTrait = Medic | Warrior
  deriving (Eq, Show)

data Player = Player
  { _playerHp :: Int,
    _playerName :: T.Text,
    _traits :: [PlayerTrait]
  }
  deriving (Show)

makeLenses ''Player

instance Character Player where
  hp = playerHp
  name = playerName
  addHp n player =
    if Medic `elem` player ^. traits
      then addHp' (n + 3) player
      else addHp' n player
