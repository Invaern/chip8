module Emulator.Monad where

import Emulator.CPU (Timer)
import Emulator.Registers (Register)

import Data.Word (Word8, Word16)

class (Monad m) => MonadEmulator m where
    load :: Word16 -> m Word8
    store :: Word8 -> Word16 -> m()

    readPC :: m Word16
    writePC :: Word16 -> m ()

    readTimer :: Timer -> m Word8
    writeTimer :: Timer -> Word8 -> m ()

    pushStack :: Word16 -> m ()
    popStack :: m Word16

    clearScreen :: m ()
    draw :: Word8 -> Word8 -> Word8 -> m Bool
    rand :: m Word8

    writeRegister :: Register -> Word8 -> m ()
    readRegister :: Register -> m Word8

    writeIP :: Word16 -> m ()
    readIP :: m Word16



class (Monad m) => System m where
    render :: m ()
    handleInputs :: m Bool
    currentMilis :: m Double
    delayMilis :: Double -> m ()

