{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Lib where

import Control.Lens
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game.GameState
import System.IO

hrChoiceGen :: [T.Text] -> T.Text
hrChoiceGen choices =
  T.intercalate ", " (init choices) `T.append` " or " `T.append` last choices

-- this is written this way so you can use functions as applicatives; doPrompt <*> promptGen $ ...
-- the constructors should be of type [T.Text] -> GameState -> T.Text
doMultiPrompt :: [T.Text] -> (GameState -> T.Text) -> StateT GameState IO T.Text
doMultiPrompt actions promptfn = do
  gamestate <- get
  lift $ do
    T.putStr $ promptfn gamestate

    hFlush stdout
    choice <- T.getLine
    if choice `elem` actions
      then return choice
      else
        putStr "That's not an option. "
          >> evalStateT (doMultiPrompt actions promptfn) gamestate

doOpenPrompt :: (GameState -> T.Text) -> StateT GameState IO T.Text
doOpenPrompt promptfn = do
  gamestate <- get
  lift $ do
    T.putStr $ promptfn gamestate
    hFlush stdout
    T.getLine
