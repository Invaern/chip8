{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Lib
import Emulator
import Emulator.Monad.ST
import Emulator.Monad.IO

main :: IO ()
main = do
    renderer <- getRenderer
    runIOEmulator renderer mainLoop
    return ()


getRenderer :: IO Renderer
getRenderer = do
    initializeAll
    window <- createWindow "My SDL Application" window
    createRenderer window (-1) defaultRenderer
  where
    window = defaultWindow 
        { windowInitialSize = V2 640 320
        }