module Game.Printers where

import System.IO

import Game.Lib

storyFmt :: String -> String
storyFmt = (:) ' ' . (++ "\n")
storyPrint :: String -> IO ()
storyPrint = (>> hFlush stdout) . putStr . storyFmt
