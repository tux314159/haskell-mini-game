{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import Game.Character.Character
import Game.Character.Monster
import Game.Character.Player
import Game.GameState
import Game.Lib
import Game.Printers
import System.Exit
import System.Random
import Text.Printf

actionPromptCons :: [T.Text] -> GameState -> T.Text
actionPromptCons actions gamestate =
  T.pack $
    printf
      "You can %s. What do you do?\n[HP: %d/100]> "
      (T.unpack $ hrChoiceGen actions)
      (gamestate ^. player . characterHp)

gameloop :: StateT GameState IO ()
gameloop = do
  gamestate <- get

  -- player's turn
  st <-
    lift $
      runStateT
        (doMultiPrompt <*> actionPromptCons $ ["attack", "heal", "idle", "run"])
        gamestate

  let (action, nst) = st
   in case action of
        "attack" -> do
          lift $ storyPrint "You attack the monster."
          r <- getStdRandom (randomR (-10, -1))
          monster %= characterAddHp r
          lift . storyPrint . printf "Monster's HP: %d/100" $ gamestate ^. player . characterHp
        "heal" -> do
          lift $ storyPrint "You heal a bit."
          r <- getStdRandom (randomR (4, 8))
          player %= characterAddHp r
        "idle" -> do
          lift $ storyPrint "You just stand there."
        "run" -> lift do
          storyPrint "You run away."
          exitSuccess
        _ -> error "Impossible!"

  mhp <- use $ monster . characterHp
  when (mhp <= 0) $ lift do
    storyPrint "You slay the monster."
    exitSuccess

  -- monster's turn
  lift $ storyPrint "The monster swings its club at you."
  r <- getStdRandom (randomR (-10, -1))
  player %= characterAddHp r

  php <- use $ player . characterHp
  when (php <= 0) $ lift do
    storyPrint "You have been killed by the monster."
    exitFailure

  gameloop

initialState :: GameState
initialState =
  GameState
    { _player =
        Player
          { _playerHp = 100,
            _playerName = ""
          },
      _monster =
        Monster
          { _monsterHp = 100,
            _monsterName = ""
          }
    }

main :: IO ()
main =
  evalStateT
    gameloop
    initialState
