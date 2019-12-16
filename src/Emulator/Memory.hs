module Emulator.Memory  (Memory, new, store, load, loadRom, addressableStart) where
import           Control.Monad               (forM_)
import           Control.Monad.ST
import           Data.Bits
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16, Word8)

newtype Memory s = Memory (V.MVector s Word8)

addressableStart :: Int
addressableStart = 0x200

toAddress :: Word16 -> Int
toAddress address = fromIntegral $ address `rem` 4096

load :: Memory s -> Word16 -> ST s Word8
load (Memory mem) address =  V.read mem $ toAddress address

store :: Memory s -> Word16 -> Word8 -> ST s ()
store (Memory mem) address = V.write mem (toAddress address)

new :: ST s (Memory s)
new = do
    vec <- V.new 4096
    let memory = Memory vec
    initFonts memory
    return memory

loadRom :: Memory s -> ByteString -> ST s ()
loadRom (Memory mem) rom = do
  let addressLimit = min (0xFFF - addressableStart) (pred $ BS.length rom)
  forM_ [0..addressLimit] $ \i -> do
      let val = BS.index rom i
      V.write mem (i + addressableStart) val


initFonts :: Memory s -> ST s ()
initFonts (Memory mem) = do
    writeChar 0x0 (0xF0, 0x90, 0x90, 0x90, 0xF0)
    writeChar 0x1 (0x20, 0x60, 0x20, 0x20, 0x70)
    writeChar 0x2 (0xF0, 0x10, 0xF0, 0x80, 0xF0)
    writeChar 0x3 (0xF0, 0x10, 0xF0, 0x10, 0xF0)
    writeChar 0x4 (0x90, 0x90, 0xF0, 0x10, 0x10)
    writeChar 0x5 (0xF0, 0x80, 0xF0, 0x10, 0xF0)
    writeChar 0x6 (0xF0, 0x80, 0xF0, 0x90, 0xF0)
    writeChar 0x7 (0xF0, 0x10, 0x20, 0x40, 0x40)
    writeChar 0x8 (0xF0, 0x90, 0xF0, 0x90, 0xF0)
    writeChar 0x9 (0xF0, 0x90, 0xF0, 0x10, 0xF0)
    writeChar 0xA (0xF0, 0x90, 0xF0, 0x90, 0x90)
    writeChar 0xB (0xE0, 0x90, 0xE0, 0x90, 0xE0)
    writeChar 0xC (0xF0, 0x80, 0x80, 0x80, 0xF0)
    writeChar 0xD (0xE0, 0x90, 0x90, 0x90, 0xE0)
    writeChar 0xE (0xF0, 0x80, 0xF0, 0x80, 0xF0)
    writeChar 0xF (0xF0, 0x80, 0xF0, 0x80, 0x80)

  where
    -- writeChar :: Int -> (Word8, Word8, Word8, Word8, Word8) -> ST s ()
    writeChar idx (a, b, c, d, e) = do
        let idx' = idx * 5
        V.write mem idx' a
        V.write mem (idx'+1) b
        V.write mem (idx'+2) c
        V.write mem (idx'+3) d
        V.write mem (idx'+4) e
