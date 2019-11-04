{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emulator.Monad.IO
    ( IOEmulator
    , runIOEmulator
    ) where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (when)
import           Control.Monad.Reader (ReaderT, ask, reader, runReaderT)
import           Control.Monad.ST     (RealWorld, stToIO)
import           Control.Monad.Trans  (MonadIO, lift)
import           Data.Word            (Word8)
import           SDL                  as SDL
import           SDL.Time             (time)

import           Emulator.CPU         (CPU)
import qualified Emulator.CPU         as CPU
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
       lift $ stToIO $ pc

    writePC !val = IOEmulator $ do
        cpu <- reader s_cpu
        lift $ stToIO $ CPU.writePC cpu val

    readIP = IOEmulator $ do
       ip <- reader (CPU.readIP . s_cpu)
       lift $ stToIO $ ip

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
        renderer <- reader s_renderer
        events <- pollEvents
        let eventIsQPress event =
              case eventPayload event of
                KeyboardEvent keyboardEvent ->
                  keyboardEventKeyMotion keyboardEvent == Pressed &&
                  keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
                QuitEvent -> True
                _ -> False
            qPressed = any eventIsQPress events
        return  qPressed

    currentMilis = IOEmulator $ do
        currentSeconds <- time
        return $ toMilis currentSeconds
        where
            toMilis seconds =  seconds * 1000

    delayMilis milis = IOEmulator $ do
        lift $ threadDelay ( floor $ milis * 1000)

runIOEmulator :: Renderer -> IOEmulator a -> IO a
runIOEmulator renderer  (IOEmulator reader) = do
    cpu <- stToIO CPU.new
    runReaderT reader (SystemState cpu renderer)
