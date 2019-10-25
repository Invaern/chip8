{-# LANGUAGE BangPatterns #-}
module Emulator.Video where
import Control.Monad (when, forM)
import           Control.Monad.ST
import           Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16, Word8)

data Video s = Video (V.MVector s Word8)

toAddress :: Word16 -> Int
toAddress address = fromIntegral $ 0x0FFF .&. address

load :: Video s -> Word16 -> ST s Word8
load (Video mem) address =  V.read mem $ (toAddress address)

store :: Video s -> Word8 -> Word16 -> ST s ()
store (Video mem) val address = V.write mem (toAddress address) val

new :: ST s (Video s)
new = do
    vec <- V.new 256
    V.write vec 0 0xA5
    return $ Video vec

activePoints :: Video s -> ST s ([(Int, Int)])
activePoints (Video vbuffer) = do
  !points <- forM [0..255] $ \idx -> do
    val <- V.read vbuffer idx
    return $ activePoints' idx val
  return $ concat points

activePoints' :: Int -> Word8 -> [(Int, Int)]
activePoints' idx val = 
  let y = idx `mod` 32
      xBase = idx `mod` 8
      !onBits = [ offset  | offset <- [0..7], (shiftL 1 offset) .&. val /= 0 ]

  in map (\bit -> (xBase + bit, y)) onBits

clear :: Video s -> ST s ()
clear (Video vbuffer) = V.set vbuffer 0