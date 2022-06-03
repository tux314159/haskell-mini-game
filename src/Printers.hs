module Printers where

import System.IO

import Lib

storyFmt :: String -> String
storyFmt = (:) ' ' . (++ "\n")
storyPrint :: String -> IO ()
storyPrint = (>> hFlush stdout) . putStr . storyFmt
