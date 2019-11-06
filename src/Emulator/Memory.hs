module Emulator.Memory  (Memory, new, store, load) where
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16, Word8)

data Memory s = Memory (V.MVector s Word8)


toAddress :: Word16 -> Int
toAddress address = fromIntegral $ address `rem` 4096

load :: Memory s -> Word16 -> ST s Word8
load (Memory mem) address =  V.read mem $ (toAddress address)

store :: Memory s -> Word16 -> Word8 -> ST s ()
store (Memory mem) address val = V.write mem (toAddress address) val

new :: ST s (Memory s)
new = do
    vec <- V.new 4096
    return $ Memory vec
