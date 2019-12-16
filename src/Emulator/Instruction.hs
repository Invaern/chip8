{-# LANGUAGE BangPatterns #-}
module Emulator.Instruction where


import           Data.Bits
import           Data.Word    (Word16, Word8)
import           Emulator.Registers (Register (..))

data Instruction
    = Call !Word16
    | ClearScreen
    | Return
    | Jump !Word16
    | SkipEq !Register !Word8
    | SkipNe !Register !Word8
    | SkipRegEq !Register !Register
    | SkipRegNe !Register !Register
    | SetRegW8 !Register !Word8
    | AddRegW8 !Register !Word8
    | SetRegReg !Register !Register
    | OrRegReg !Register !Register
    | AndRegReg !Register !Register
    | XorRegReg !Register !Register
    | AddRegReg !Register !Register
    | SubReg1Reg2 !Register !Register
    | SubReg2Reg1 !Register !Register
    | LsbShiftR !Register
    | MsbShiftL !Register
    | SetI !Word16
    | JumpV0 !Word16
    | SetRandAnd !Register !Word8
    | Draw !Register !Register !Word8
    | SkipKeyPressed !Register
    | SkipKeyNotPressed !Register
    | GetDelay !Register
    | StoreKey !Register
    | SetDelay !Register
    | SetSound !Register
    | AddI !Register
    | SetChar !Register
    | BCD !Register
    | RegDump !Register
    | RegLoad !Register
    | NoOp
    | Unknown !Word8 !Word8
    deriving (Show)

decodeInstruction :: Word8 -> Word8 -> Instruction
decodeInstruction op1 op2 = case (a, b, c, d) of
    (0, 0, 0xE, 0)     -> ClearScreen
    (0, 0, 0xE, 0xE)   -> Return
    (1, x, y, z)       -> Jump $ word12 x y z
    (2, x, y, z)       -> Call $ word12 x y z
    (3, x, _, _)       -> SkipEq (reg x) op2
    (4, x, _, _)       -> SkipNe (reg x) op2
    (5, x, y, 0)       -> SkipRegEq (reg x) (reg y)
    (6, x, _, _)       -> SetRegW8 (reg x) op2
    (7, x, _, _)       -> AddRegW8 (reg x) op2
    (8, x, y, 0)       -> SetRegReg (reg x) (reg y)
    (8, x, y, 1)       -> OrRegReg (reg x) (reg y)
    (8, x, y, 2)       -> AndRegReg (reg x) (reg y)
    (8, x, y, 3)       -> XorRegReg (reg x) (reg y)
    (8, x, y, 4)       -> AddRegReg (reg x) (reg y)
    (8, x, y, 5)       -> SubReg1Reg2 (reg x) (reg y)
    (8, x, _, 6)       -> LsbShiftR (reg x)
    (8, x, y, 7)       -> SubReg2Reg1 (reg x) (reg y)
    (8, x, _, 0xE)     -> MsbShiftL (reg x)
    (9, x, y, 0)       -> SkipRegNe (reg x) (reg y)
    (0xA, x, y, z)     -> SetI $ word12 x y z
    (0xB, x, y, z)     -> JumpV0 $ word12 x y z
    (0xC, x, _, _)     -> SetRandAnd (reg x) op2
    (0xD, x, y, z)     -> Draw (reg x) (reg y) z
    (0xE, x, 0x9, 0xE) -> SkipKeyPressed (reg x)
    (0xE, x, 0xA, 0x1) -> SkipKeyNotPressed (reg x)
    (0xF, x, 0x0, 0x7) -> GetDelay (reg x)
    (0xF, x, 0x0, 0xA) -> StoreKey (reg x)
    (0xF, x, 0x1, 0x5) -> SetDelay (reg x)
    (0xF, x, 0x1, 0x8) -> SetSound (reg x)
    (0xF, x, 0x1, 0xE) -> AddI (reg x)
    (0xF, x, 0x2, 0x9) -> SetChar (reg x)
    (0xF, x, 0x3, 0x3) -> BCD (reg x)
    (0xF, x, 0x5, 0x5) -> RegDump (reg x)
    (0xF, x, 0x6, 0x5) -> RegLoad (reg x)
    (0, _, _, _)       -> NoOp
    _                  -> Unknown op1 op2
  where
    a = shiftR op1 4
    b = op1 .&. 0x0F
    c = shiftR op2 4
    d = op2 .&. 0x0F
    word12 x y z = (fromIntegral x `shiftL` 8) .|. (fromIntegral y `shiftL` 4) .|. fromIntegral z
    word8 x y = (fromIntegral x `shiftL` 4) .|. fromIntegral y
    reg 0x0 = V0
    reg 0x1 = V1
    reg 0x2 = V2
    reg 0x3 = V3
    reg 0x4 = V4
    reg 0x5 = V5
    reg 0x6 = V6
    reg 0x7 = V7
    reg 0x8 = V8
    reg 0x9 = V9
    reg 0xA = VA
    reg 0xB = VB
    reg 0xC = VC
    reg 0xD = VD
    reg 0xE = VE
    reg 0xF = VF
    reg x = error $ "unknown register " ++ show x
