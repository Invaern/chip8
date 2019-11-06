{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emulator.Monad.IO
    ( IOEmulator
    , runIOEmulator
    ) where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM_, guard, when)
import           Control.Monad.Reader (ReaderT, ask, reader, runReaderT)
import           Control.Monad.ST     (RealWorld, stToIO)
import           Control.Monad.Trans  (MonadIO, lift)
import           Data.Maybe           (mapMaybe)
import           Data.Word            (Word8)
import           SDL                  
import           SDL.Time             (time)
import           System.Random        (randomIO)


import           Emulator.CPU         (CPU)
import qualified Emulator.CPU         as CPU
import qualified Emulator.Keyboard    as Keyboard
import qualified Emulator.Memory      as Memory
import           Emulator.Monad
import qualified Emulator.Registers   as Registers
import           Emulator.System      (SystemState (..))
import qualified Emulator.Video       as Video

newtype IOEmulator a = IOEmulator (ReaderT (SystemState RealWorld) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadEmulator IOEmulator where
    load !address = IOEmulator $ do
        mem <- reader (CPU.memory . s_cpu)
        !loaded <- lift $ stToIO $ Memory.load mem address
        -- lift $ print loaded
        return loaded
    store !address word = IOEmulator $ do
        mem <- reader (CPU.memory . s_cpu)
        lift $ stToIO $ Memory.store mem address word

    readPC = IOEmulator $ do
       pc <- reader (CPU.readPC . s_cpu)
       lift $ stToIO pc

    writePC !val = IOEmulator $ do
        cpu <- reader s_cpu
        lift $ stToIO $ CPU.writePC cpu val

    readIP = IOEmulator $ do
       ip <- reader (CPU.readIP . s_cpu)
       lift $ stToIO ip

    writeIP !val = IOEmulator $ do
        cpu <- reader s_cpu
        lift $ stToIO $ CPU.writeIP cpu val

    readTimer timer = IOEmulator $ do
        cpu <- reader  s_cpu
        lift $ stToIO $ CPU.readTimer cpu timer

    writeTimer timer val = IOEmulator $ do
        cpu <- reader  s_cpu
        lift $ stToIO $ CPU.writeTimer cpu timer val

    clearScreen = IOEmulator $ do
        video <- reader (CPU.video . s_cpu)
        lift $ stToIO $ Video.clear video

    draw x y val = IOEmulator $ do
        video <- reader (CPU.video . s_cpu)
        lift $ stToIO $ Video.draw video x y val

    readRegister reg = IOEmulator $ do
        registers <- reader (CPU.registers . s_cpu)
        lift $ stToIO $ Registers.read registers reg

    writeRegister reg val = IOEmulator $ do
        registers <- reader (CPU.registers . s_cpu)
        lift $ stToIO $ Registers.write registers reg val

    testKey key = IOEmulator $ do
        keyboard <- reader (CPU.keyboard . s_cpu)
        lift $ stToIO $ Keyboard.isOn keyboard key

    pushStack val = IOEmulator $ do
        cpu <- reader s_cpu
        lift $ stToIO $ CPU.pushStack cpu val

    popStack = IOEmulator $ do
        cpu <- reader s_cpu
        lift $ stToIO $ CPU.popStack cpu

    rand = IOEmulator $ lift randomIO


instance System IOEmulator where
    render = IOEmulator $ do
        renderer <- reader s_renderer
        rendererDrawColor renderer $= V4 0x0 0x0 0x0 0xFF
        clear renderer
        rendererDrawColor renderer $= V4 0xF0 0xF0 0xF0 0xFF
        vbuffer <- reader (CPU.video . s_cpu)
        points <- lift $ stToIO $ Video.activePoints vbuffer
        mapM_ (renderCell renderer) points
        present renderer
        where
            renderCell :: (MonadIO m) => Renderer -> (Int, Int) -> m ()
            renderCell renderer (x,y) = do
                let (x', y') = (fromIntegral x*10, fromIntegral y*10)
                    rect = Rectangle (P $ V2 x' y') (V2 10 10)
                fillRect renderer (Just rect)



    handleInputs = IOEmulator $ do
        events <- pollEvents
        let quitEvent = any isQuitEvent events
            pressedKeys = mapMaybe pressedKey events

        keyboard <- reader (CPU.keyboard . s_cpu)
        forM_ pressedKeys (lift . stToIO . Keyboard.set keyboard)

        return quitEvent
      where
        pressedKey :: Event -> Maybe Word8
        pressedKey event = case eventPayload event of
            KeyboardEvent keyboardEvent -> do
                guard $ keyboardEventKeyMotion keyboardEvent == Pressed
                mapKeycode $ keysymKeycode (keyboardEventKeysym keyboardEvent)
            _ ->  Nothing

        isQuitEvent event = case eventPayload event of
            QuitEvent -> True
            _         -> False

    currentMilis = IOEmulator $ toMilis <$> time
        where
            toMilis seconds =  seconds * 1000

    delayMilis milis = IOEmulator $ lift $ threadDelay (floor $ milis * 1000)

runIOEmulator :: Renderer -> IOEmulator a -> IO a
runIOEmulator renderer  (IOEmulator reader) = do
    cpu <- stToIO CPU.new
    runReaderT reader (SystemState cpu renderer)

mapKeycode :: Keycode -> Maybe Word8
mapKeycode Keycode1 = Just 0
mapKeycode Keycode2 = Just 1
mapKeycode Keycode3 = Just 2
mapKeycode Keycode4 = Just 3
mapKeycode KeycodeQ = Just 4
mapKeycode KeycodeW = Just 5
mapKeycode KeycodeE = Just 6
mapKeycode KeycodeR = Just 7
mapKeycode KeycodeA = Just 8
mapKeycode KeycodeS = Just 9
mapKeycode KeycodeD = Just 10
mapKeycode KeycodeF = Just 11
mapKeycode KeycodeZ = Just 12
mapKeycode KeycodeX = Just 13
mapKeycode KeycodeC = Just 14
mapKeycode KeycodeV = Just 15
mapKeycode _        = Nothing
