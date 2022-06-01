{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.State
import Data.List (intercalate)
import System.IO
import Text.Printf
import Data.Functor.Identity (Identity)
import Control.Lens

data GameState = GameState
  { _playerHp :: Int,
    _monsterHp :: Int
  }
makeLenses ''GameState

doPrompt :: [String] -> StateT GameState IO String
doPrompt actions =
  let actstr = intercalate ", " (init actions) ++ " or " ++ last actions
   in do
        gamestate <- get
        lift $ do
          putStr (printf "You can %s. What do you do?\n[HP: %d/100]> " actstr (_playerHp gamestate))
          hFlush stdout
          choice <- getLine
          if choice `elem` actions
             then return choice
             else putStr "That's not an option. " >> evalStateT (doPrompt actions) gamestate

playerAddHp :: Int -> StateT GameState IO ()
playerAddHp n = do
  php <- gets _playerHp
  if php + n > 100
     then playerHp .= 100
     else if php + n < 0
             then playerHp .= 0
             else playerHp += n
  return ()

monsterAddHp :: Int -> StateT GameState IO ()
monsterAddHp n = do
  mhp <- gets _monsterHp
  if mhp + n > 100
     then monsterHp .= 100
     else if mhp + n < 0
             then monsterHp .= 0
             else monsterHp += n
  return ()
