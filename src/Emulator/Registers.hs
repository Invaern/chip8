module Emulator.Registers (Registers, write, read, new, Register(..)) where
import           Control.Monad.ST            (ST)
import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Word                   (Word8)
import           Prelude                     hiding (read)

data Registers s = Registers (V.MVector s Word8)

write :: Registers s -> Register -> Word8 -> ST s ()
write (Registers regs) reg val = V.write regs (registerIdx reg) val

read :: Registers s -> Register -> ST s Word8
read (Registers regs) reg = V.read regs (registerIdx reg)

new :: ST s (Registers s)
new = do
    vec <- V.new 16
    return $ Registers vec

registerIdx :: Register -> Int
registerIdx V0 = 0
registerIdx V1 = 1
registerIdx V2 = 2
registerIdx V3 = 3
registerIdx V4 = 4
registerIdx V5 = 5
registerIdx V6 = 6
registerIdx V7 = 7
registerIdx V8 = 8
registerIdx V9 = 9
registerIdx VA = 10
registerIdx VB = 11
registerIdx VC = 12
registerIdx VD = 13
registerIdx VE = 14
registerIdx VF = 15

data Register
    = V0 | V1 | V2 | V3
    | V4 | V5 | V6 | V7
    | V8 | V9 | VA | VB
    | VC | VD | VE | VF