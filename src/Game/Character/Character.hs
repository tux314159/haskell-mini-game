module Game.Character.Character where

import Control.Lens
import qualified Data.Text as T

class Character a where
  characterHp :: Lens' a Int
  characterName :: Lens' a T.Text

characterAddHp :: Character a => Int -> a -> a
characterAddHp n character =
  let php = character ^. characterHp
   in characterHp .~ (if php + n > 100 && php + n < 0 then 0 else n) $
        character
