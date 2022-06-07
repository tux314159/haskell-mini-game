module Game.Character.Character where

import Control.Lens
import qualified Data.Text as T

class Character a where
  hp :: Lens' a Int
  name :: Lens' a T.Text
  addHp :: Int -> a -> a

-- the standard hp adding
addHp' :: Character a => Int -> a -> a
addHp' n character
  | chp + n < 0 = sethp 0
  | chp + n > 100 = sethp 100
  | otherwise = sethp $ chp + n
  where
    chp = character ^. hp
    sethp n = hp .~ chp $ character
