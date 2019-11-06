{-# LANGUAGE NamedFieldPuns #-}
module Emulator.CPU (CPU, new, memory, readPC, writePC,
                     Timer(..), writeTimer, readTimer,
                     readIP, writeIP, pushStack, popStack,
                     video, registers, keyboard)  where

import           Control.Monad.ST            (ST)
import           Data.Mutable
import           Data.STRef                  (modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word16, Word8)
import           Emulator.Keyboard           (Keyboard)
import qualified Emulator.Keyboard           as Keyboard
import           Emulator.Memory             (Memory)
import qualified Emulator.Memory             as Memory
import           Emulator.Registers          (Registers)
import qualified Emulator.Registers          as Registers
import           Emulator.Video              (Video)
import qualified Emulator.Video              as Video

data CPU s = CPU
    { c_memory    :: Memory s
    , c_registers :: Registers s
    , c_keyboard :: Keyboard s
    , c_pc        :: URef s Word16
    , c_delay_t   :: URef s Word8
    , c_sound_t   :: URef s Word8
    , c_ip        :: URef s Word16
    , c_stack     :: STRef s [Word16]
    , c_vbuffer   :: Video s
    }


data Timer = DelayTimer | SoundTimer


new :: ST s (CPU s)
new = do
    mem <- Memory.new
    pc <- newRef 0
    ip <- newRef 0
    delay_t <- newRef 0
    sound_t <- newRef 0
    stack <- newSTRef []
    vbuffer <- Video.new'
    regs <- Registers.new
    keyboard <- Keyboard.new
    return $ CPU
        { c_memory = mem
        , c_pc = pc
        , c_ip = ip
        , c_delay_t = delay_t
        , c_sound_t = sound_t
        , c_stack = stack
        , c_vbuffer = vbuffer
        , c_registers = regs
        , c_keyboard = keyboard
        }

memory :: CPU s -> Memory s
memory cpu = c_memory cpu

video :: CPU s -> Video s
video cpu = c_vbuffer cpu

registers :: CPU s -> Registers s
registers cpu = c_registers cpu

keyboard :: CPU s -> Keyboard s
keyboard cpu = c_keyboard cpu

readIP :: CPU s -> ST s Word16
readIP cpu = readRef $ c_ip cpu

readPC :: CPU s -> ST s Word16
readPC cpu = readRef $ c_pc cpu

writePC :: CPU s -> Word16 -> ST s ()
writePC cpu val = writeRef (c_pc cpu) val

writeIP :: CPU s -> Word16 -> ST s ()
writeIP cpu val = writeRef (c_ip cpu) val

readTimer :: CPU s -> Timer -> ST s Word8
readTimer CPU {c_delay_t} DelayTimer = readRef c_delay_t
readTimer CPU {c_sound_t} SoundTimer = readRef c_sound_t

writeTimer :: CPU s -> Timer -> Word8 -> ST s ()
writeTimer cpu DelayTimer val = writeRef (c_delay_t cpu) val
writeTimer cpu SoundTimer val = writeRef (c_sound_t cpu) val


pushStack :: CPU s -> Word16 -> ST s ()
pushStack CPU {c_stack} val = modifySTRef' c_stack (val:)

popStack :: CPU s -> ST s Word16
popStack CPU {c_stack} = do
    stack <- readSTRef c_stack
    case stack of
        (x:xs) -> do
            writeSTRef c_stack xs
            return x
        _ -> fail "Popping from empty stack"
