{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Lens
import Control.Monad.State
import Lib
import System.Exit
import Text.Printf
import Control.Monad

gameloop :: StateT GameState IO ()
gameloop = do
  gamestate <- get

  -- player's turn
  st <- lift $ runStateT (doPrompt ["attack", "heal", "idle", "run"]) gamestate
  let (action, nst) = st
   in case action of
        "attack" -> do
          lift $ putStrLn "You attack the monster."
          lift (execStateT (monsterAddHp (-10)) gamestate) >>= put
          newstate <- get
          lift $ putStrLn $ printf "Monster's HP: %d/100" $ _monsterHp newstate
        "heal" -> do
          lift $ putStrLn "You heal a bit."
          lift (execStateT (playerAddHp 5) gamestate) >>= put
        "idle" -> do
          lift $ putStrLn "You just stand there."
        "run" -> do
          lift $ putStrLn "You run away."
          lift exitSuccess
        _ -> error "Impossible!"

  mhp <- use monsterHp
  when (mhp <= 0) $ lift do
       putStrLn "You slay the monster."
       exitSuccess

  -- monster's turn
  lift $ putStrLn "The monster swings its club at you."
  playerAddHp (-4)

  php <- use playerHp
  when (php <= 0) $ lift do
       putStrLn "You have been killed by the monster."
       exitFailure

  gameloop



main :: IO ()
main = evalStateT gameloop (GameState {_playerHp = 100, _monsterHp = 100})
