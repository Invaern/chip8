{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Emulator (mainLoop)
import Emulator.Monad.IO (runIOEmulator)
import Config (opts)
import Options.Applicative (execParser)

main :: IO ()
main = do
    config <- execParser opts
    renderer <- getRenderer
    runIOEmulator renderer config mainLoop
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