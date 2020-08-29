module Emulator.Keyboard (Keyboard, KeyPress(..), new, isOn, set) where

import           Control.Monad.ST (ST)
import           Data.Mutable     (URef, newRef, readRef, modifyRef')
import           Data.Word        (Word16, Word8)
import           Data.Bits

newtype Keyboard s = Keyboard (URef s Word16)
data KeyPress = On Word8 | Off Word8

new :: ST s (Keyboard s)
new = do
    val <- newRef 0
    return $ Keyboard val

isOn :: Keyboard s -> Word8 -> ST s Bool
isOn (Keyboard mem) key = do
    val <- readRef mem
    return $ testBit val (fromIntegral key)

set :: Keyboard s -> KeyPress -> ST s ()
set (Keyboard mem) (On key) = modifyRef' mem $ \val -> setBit val (fromIntegral key)
set (Keyboard mem) (Off key) = modifyRef' mem $ \val -> clearBit val (fromIntegral key)