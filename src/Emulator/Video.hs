{-# LANGUAGE BangPatterns #-}
module Emulator.Video (Video, draw, activePoints, new, clear) where
import           Control.Monad               (forM)
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Word                   (Word8)

newtype Video s = Video (V.MVector s Word8)

new :: ST s (Video s)
new = do
    vec <- VM.new 256
    return $ Video vec

draw :: Video s -> Word8 -> Word8 -> Word8 -> ST s Bool
draw (Video mem) x y val = do
  let wrapped_y = y `rem` 32
      (x', x_shift) = x `quotRem` 8 -- for given row gets (which_byte, offset_into_byte)
      wrapped_x = x' `rem` 8
      idx = fromIntegral $ wrapped_y*8 + wrapped_x
  if x_shift == 0
    then drawAligned mem idx
    else drawNotAligned mem idx (wrapped_x, wrapped_y) x_shift

  where
    drawAligned mem idx = do
      oldVal <- VM.read mem idx
      let newVal = xor oldVal val
      VM.write mem idx newVal
      return $ collision oldVal newVal

    drawNotAligned mem idx (wrapped_x, wrapped_y) offset = do
      let lsb_x = (wrapped_x + 1) `rem` 8
          lsb_idx = fromIntegral $ wrapped_y*8 + lsb_x
      msb <- VM.read mem idx
      lsb <- VM.read mem lsb_idx
      let msbValMasked = xor msb (val `shiftR` fromIntegral offset)
          lsbValMasked = xor lsb (val `shiftL` fromIntegral (8 - offset))
      VM.write mem idx msbValMasked
      VM.write mem lsb_idx lsbValMasked
      return $ collision msb msbValMasked || collision lsb lsbValMasked


    collision :: Word8 -> Word8 -> Bool
    collision oldVal newVal = oldVal > (newVal .&. oldVal)


activePoints :: Video s -> ST s [(Int, Int)]
activePoints (Video vbuffer) = do
  !points <- forM [0..255] $ \idx -> do
    val <- VM.read vbuffer idx
    return $ activePoints' idx val
  return $ concat points

activePoints' :: Int -> Word8 -> [(Int, Int)]
activePoints' idx val =
  let (y, xBase) = idx `quotRem` 8
      x = xBase * 8
      !onBits = [ offset  | offset <- [0..7], shiftR 128 offset .&. val /= 0 ]

  in map (\bit -> (x + bit, y)) onBits

clear :: Video s -> ST s ()
clear (Video vbuffer) = VM.set vbuffer 0
