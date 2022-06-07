{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Game.Character.Character
import Game.Character.Monster
import Game.Character.Player
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcTests]

instance QC.Arbitrary Player where
  arbitrary = do
    genHp <- QC.choose (0, 100)
    return
      Player
        { _playerHp = genHp,
          _playerName = "",
          _traits = []
        }

instance QC.Arbitrary Monster where
  arbitrary = do
    genHp <- QC.choose (0, 100)
    return
      Monster
        { _monsterHp = genHp,
          _monsterName = ""
        }

qcTests :: TestTree
qcTests =
  testGroup
    "Property testing"
    [ QC.testProperty "HP adding bounds check (player)" $
        \(char :: Player) n -> let newhp = addHp n char ^. hp in newhp >= 0 && newhp <= 100,
      QC.testProperty "HP adding bounds check (monster)" $
        \(char :: Monster) n -> let newhp = addHp n char ^. hp in newhp >= 0 && newhp <= 100
    ]
