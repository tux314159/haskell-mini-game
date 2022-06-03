{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Lens
import Control.Monad.State
import System.Exit
import System.Random
import Text.Printf
import Control.Monad

import Lib
import Printers

gameloop :: StateT GameState IO ()
gameloop = do
  gamestate <- get

  -- player's turn
  st <- lift $ runStateT (doPrompt ["attack", "heal", "idle", "run"]) gamestate
  let (action, nst) = st
   in case action of
        "attack" -> do
          lift $ storyPrint "You attack the monster."
          r <- getStdRandom (randomR (-10, -1))
          put =<< lift (execStateT (monsterAddHp r) gamestate)
          lift . storyPrint . printf "Monster's HP: %d/100" . _monsterHp =<< get
        "heal" -> do
          r <- getStdRandom (randomR (4, 8))
          lift $ storyPrint "You heal a bit."
          put =<< lift (execStateT (playerAddHp r) gamestate)
        "idle" -> do
          lift $ storyPrint "You just stand there."
        "run" -> lift do
          storyPrint "You run away."
          exitSuccess
        _ -> error "Impossible!"

  mhp <- use monsterHp
  when (mhp <= 0) $ lift do
       storyPrint "You slay the monster."
       exitSuccess

  -- monster's turn
  lift $ storyPrint "The monster swings its club at you."
  playerAddHp =<< getStdRandom (randomR (-10, -1))

  php <- use playerHp
  when (php <= 0) $ lift do
       storyPrint "You have been killed by the monster."
       exitFailure

  gameloop

main :: IO ()
main = evalStateT gameloop (GameState {_playerHp = 100, _monsterHp = 100})
