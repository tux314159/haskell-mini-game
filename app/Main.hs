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

main :: IO ()
main =
  execStateT setPlayerName initialState
    >>= execStateT printIntro
      >>= evalStateT gameLoop
  where
    setPlayerName =
      get >>= \st ->
        (player . name .=)
          =<< lift (evalStateT (doOpenPrompt askNameCons) st)

    printIntro :: StateT GameState IO ()
    printIntro = do
      st <- get
      lift $
        storyPrint $
          printf
            "You are %s, with the following skills: %s"
            (st ^. player . name)
            (show $ st ^. player . traits)

initialState :: GameState
initialState =
  GameState
    { _player =
        Player
          { _playerHp = 100,
            _playerName = "",
            _traits = [Medic]
          },
      _monster =
        Monster
          { _monsterHp = 100,
            _monsterName = ""
          }
    }

-- some prompt constructors

actionPromptCons :: [T.Text] -> GameState -> T.Text
actionPromptCons actions gameState =
  T.pack $
    printf
      "You can %s. What do you do?\n[HP: %d/100]> "
      (T.unpack $ hrChoiceGen actions)
      (gameState ^. player . hp)

askNameCons :: GameState -> T.Text
askNameCons = pure "Enter your name: "

-- main game loop

gameLoop :: StateT GameState IO ()
gameLoop = do
  gameState <- get

  -- player's turn
  st <-
    lift $
      runStateT
        (doMultiPrompt <*> actionPromptCons $ ["attack", "heal", "idle", "run"])
        gameState

  let (action, nst) = st
   in case action of
        "attack" -> do
          lift $ storyPrint "You attack the monster."
          r <- getStdRandom (randomR (-10, -1))
          monster %= addHp r
          newState <- get
          lift . storyPrint . printf "Monster's HP: %d/100" $ newState ^. monster . hp
        "heal" -> do
          lift $ storyPrint "You heal a bit."
          r <- getStdRandom (randomR (4, 8))
          player %= addHp r
        "idle" -> do
          lift $ storyPrint "You just stand there."
        "run" -> lift do
          storyPrint "You run away."
          exitSuccess
        _ -> error "Impossible!"

  mhp <- use $ monster . hp
  when (mhp <= 0) $ lift do
    storyPrint "You slay the monster."
    exitSuccess

  -- monster's turn
  lift $ storyPrint "The monster swings its club at you."
  r <- getStdRandom (randomR (-10, -1))
  player %= addHp r

  php <- use $ player . hp
  when (php <= 0) $ lift do
    storyPrint "You have been killed by the monster."
    exitFailure

  gameLoop
