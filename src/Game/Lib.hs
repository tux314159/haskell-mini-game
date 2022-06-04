{-# LANGUAGE ScopedTypeVariables #-}

module Game.Lib where

import Control.Lens
import Control.Monad.State
import Data.List (intercalate)
import Game.GameState
import System.IO

hrChoiceGen :: [String] -> String
hrChoiceGen choices =
  intercalate ", " (init choices) ++ " or " ++ last choices

-- this is written this way so you can use functions as applicatives; doPrompt <*> promptGen $ ...
-- the constructors should be of type [String] -> GameState -> String
doMultiPrompt :: [String] -> (GameState -> String) -> StateT GameState IO String
doMultiPrompt actions promptfn =
  let actstr = intercalate ", " (init actions) ++ " or " ++ last actions
   in do
        gamestate <- get
        lift $ do
          putStr $ promptfn gamestate

          hFlush stdout
          choice <- getLine
          if choice `elem` actions
            then return choice
            else putStr "That's not an option. "
              >> evalStateT (doMultiPrompt actions promptfn) gamestate
