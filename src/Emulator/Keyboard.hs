module Emulator.Keyboard (Keyboard, new, isOn, set) where

-- import           Data.STRef (modifySTRef', newSTRef, readSTRef)
import           Control.Monad.ST (ST)
import           Data.Mutable     (URef, newRef, readRef, modifyRef')
import           Data.Word        (Word16, Word8)
import           Data.Bits

newtype Keyboard s = Keyboard (URef s Word16)

new :: ST s (Keyboard s)
new = do
    val <- newRef 0
    return $ Keyboard val

isOn :: Keyboard s -> Word8 -> ST s Bool
isOn (Keyboard mem) key = do
    val <- readRef mem
    return $ testBit val (fromIntegral key)

set :: Keyboard s -> Word8 -> ST s ()
set (Keyboard mem) key = modifyRef' mem $ \val -> setBit val (fromIntegral key)