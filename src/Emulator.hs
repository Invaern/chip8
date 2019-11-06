{-# LANGUAGE BangPatterns #-}
module Emulator where


import           Control.Monad        (forM, replicateM_, unless, when)
import           Data.Bits
import           Data.Word            (Word8)
import           Emulator.CPU         (Timer (..))
import           Emulator.Instruction (Instruction (..), decodeInstruction)
import           Emulator.Monad       (MonadEmulator (..), System (..))
import           Emulator.Registers   (Register (..), registersUpTo)

import           Debug.Trace

step :: (MonadEmulator m) => m ()
step = do
    val <- load 0
    store 0 (val + 1)

testRun :: MonadEmulator m => m Word8
testRun = do
    replicateM_ 100000000 (step )
    load 0

interpreterFrequency :: Double
interpreterFrequency = 500

mainLoop :: (MonadEmulator m, System m) => m ()
mainLoop = loop
  where
    loop :: (MonadEmulator m, System m) => m ()
    loop = do
        quit <- handleInputs
        unless quit $ do
            !startTime <- currentMilis
            -- traceM ("start time: " ++ show startTime)
            frameSteps stepsPerFrame
            updateTimers 
            --step
            -- stepTime <- currentMilis
            -- traceM ("step time: " ++ show stepTime)
            render
            !endTime <- currentMilis
            -- traceM ("end time: " ++ show endTime)
            throttle startTime endTime
            loop
    frameSteps :: (MonadEmulator m) => Int -> m ()
    frameSteps 0 = pure ()
    frameSteps n = do
        step
        frameSteps (n-1)

    stepsPerFrame = floor $ interpreterFrequency / fps

fps :: Double
fps = 60

timeToDraw :: Double
timeToDraw = (1/fps) * 1000

-- updateTimers :: (MonadEmulator m) => Double -> Double -> m ()
-- updateTimers startTime endTime = do
--     updateTimers' $ elapsedW8 (endTime - startTime)
--   where
--     elapsedW8 :: Double -> Word8
--     elapsedW8 elapsed | elapsed > 255 = 255
--                       | otherwise = floor elapsed


throttle :: (System m ) => Double -> Double ->  m ()
throttle startTime endTime = do
    let elapsed = endTime - startTime
        sleepTime = delayInMs elapsed
    -- traceM ("elapsed: " ++ show elapsed)
    -- traceM("time to draw: " ++ show timeToDraw)
    -- traceM ("sleeping for " ++ show sleepTime)
    delayMilis sleepTime
    return ()
  where
    delayInMs elapsed | elapsed > timeToDraw = 0
                      | otherwise = timeToDraw - elapsed


nextInstruction :: MonadEmulator m => m Instruction
nextInstruction = do
    pc <- readPC
    op1 <- load pc
    op2 <- load (pc + 1)
    writePC (pc + 2)
    return $ decodeInstruction op1 op2


updateTimers :: MonadEmulator m => m ()
updateTimers = do
    delayT <- readTimer DelayTimer
    soundT <- readTimer SoundTimer
    when (delayT > 0) $ writeTimer DelayTimer (delayT - 1)
    when (soundT > 0) $ writeTimer SoundTimer (soundT - 1)


executeInstruction :: MonadEmulator m => Instruction -> m ()
executeInstruction (Call address) = do
    pc <- readPC
    pushStack pc
    writePC address

executeInstruction ClearScreen = clearScreen

executeInstruction Return = do
    pc' <- popStack
    writePC pc'

executeInstruction (Jump address) = writePC address

executeInstruction (SkipEq reg val) = do
    regVal <- readRegister reg
    when (regVal == val) $ do
        pc <- readPC
        writePC (pc + 2)

executeInstruction (SkipNe reg val) = do
    regVal <- readRegister reg
    when (regVal /= val) $ do
        pc <- readPC
        writePC (pc + 2)

executeInstruction (SkipRegEq reg1 reg2) = do
    reg1Val <- readRegister reg1
    reg2Val <- readRegister reg2
    when (reg1Val == reg2Val) $ do
        pc <- readPC
        writePC (pc + 2)

executeInstruction (SkipRegNe reg1 reg2) = do
    reg1Val <- readRegister reg1
    reg2Val <- readRegister reg2
    when (reg1Val /= reg2Val) $ do
        pc <- readPC
        writePC (pc + 2)

executeInstruction (SetRegW8 reg val) = writeRegister reg val

executeInstruction (AddRegW8 reg val) = do
    oldVal <- readRegister reg
    let newVal = oldVal + val
    writeRegister reg newVal
    when (newVal < oldVal && newVal < val) (writeRegister VF 1)

executeInstruction (SetRegReg target source) = do
    sourceVal <- readRegister source
    writeRegister target sourceVal

executeInstruction(OrRegReg target source) = do
    sourceVal <- readRegister source
    targetVal <- readRegister target
    writeRegister target (sourceVal .|. targetVal)

executeInstruction(AndRegReg target source) = do
    sourceVal <- readRegister source
    targetVal <- readRegister target
    writeRegister target (sourceVal .&. targetVal)

executeInstruction(XorRegReg target source) = do
    sourceVal <- readRegister source
    targetVal <- readRegister target
    writeRegister target (xor sourceVal targetVal)

executeInstruction (SubReg1Reg2 target source) = do
    sourceVal <- readRegister source
    targetVal <- readRegister target
    writeRegister target (targetVal - sourceVal)
    if (sourceVal > targetVal)
        then writeRegister VF 0
        else writeRegister VF 1

executeInstruction (SubReg2Reg1 target source) = do
    sourceVal <- readRegister source
    targetVal <- readRegister target
    writeRegister target (sourceVal - targetVal)
    if (targetVal > sourceVal)
        then writeRegister VF 0
        else writeRegister VF 1

executeInstruction (LsbShiftR reg) = do
    val <- readRegister reg
    let lsb = val .&. 1
    writeRegister reg (val `shiftR` 1)
    writeRegister VF lsb

executeInstruction (MsbShiftL reg) = do
    val <- readRegister reg
    let msb = val .&. 0x80
    writeRegister reg (val `shiftL` 1)
    writeRegister VF msb

executeInstruction (SetI val) = writeIP val

executeInstruction (JumpV0 val) = do
    v0 <- readRegister V0
    writePC (fromIntegral v0 + val)

executeInstruction (SetRandAnd reg val) = do
    randVal <- rand
    writeRegister reg (val .&. randVal)

executeInstruction (Draw regX regY rows) = do
    x <- readRegister regX
    y <- readRegister regY
    ip <- readIP
    collisions <- forM [0..(rows-1)] $ \row -> do
        val <- load (ip + fromIntegral row)
        draw x (y+row) val
    when (or collisions) (writeRegister VF 1)

executeInstruction (SkipKeyPressed reg) = do
    key <- readRegister reg
    on <- testKey key
    when on $ do
        pc <- readPC
        writePC (pc + 2)


executeInstruction (SkipKeyNotPressed reg) = do
    key <- readRegister reg
    on <- testKey key
    unless on $ do
        pc <- readPC
        writePC (pc + 2)

executeInstruction (GetDelay reg) = do
    delay <- readTimer DelayTimer
    writeRegister reg delay

executeInstruction (StoreKey reg) = do --TODO: implement some blocking
    pressedKey <- anyKeyPressed
    case pressedKey of
        Nothing  -> rollback
        Just key -> setKey key
  where
    rollback = do
        pc <- readPC
        writePC (pc-2)
    setKey key = writeRegister reg key

executeInstruction (SetDelay reg) = do
    val <- readRegister reg
    writeTimer DelayTimer val

executeInstruction (SetSound reg) = do
    val <- readRegister reg
    writeTimer SoundTimer val

executeInstruction (AddI reg) = do
    ip <- readIP
    val <- readRegister reg
    let newIP = ip + fromIntegral val
    when (newIP > 0xFFF ) (writeRegister VF 1)
    writeIP (newIP `rem` 0xFFF)

executeInstruction (SetChar reg) = do
    val <- readRegister reg
    let newIP = 4 * (val .&. 0xF)
    writeIP (fromIntegral newIP)

executeInstruction (BCD reg) = do
    val <- readRegister reg
    let hundreds = getHundreds val
        tens = getTens val
        ones = getOnes val
    ip <- readIP
    store ip hundreds
    store (ip+1) tens
    store (ip+2) ones
  where
    getHundreds = (`rem` 10) . (`div` 100)
    getTens = (`rem` 10) . (`div` 10)
    getOnes = (`rem` 10)

executeInstruction (RegDump reg) = do
    let registers = registersUpTo reg
    ip <- readIP
    dumpRegs ip registers
  where
    dumpRegs address [] = pure ()
    dumpRegs address (r:regs) = do
        val <- readRegister r
        store address val
        dumpRegs (address + 1) regs

executeInstruction (RegLoad reg) = do
    let registers = registersUpTo reg
    ip <- readIP
    loadRegs ip registers
  where
    loadRegs address [] = pure ()
    loadRegs address (r:regs) = do
        val <- load address
        writeRegister r val
        loadRegs (address + 1) regs

executeInstruction Unknown = fail "Unknown instruction"

anyKeyPressed :: (MonadEmulator m) => m (Maybe Word8)
anyKeyPressed = key 0
  where
    key 16 = return Nothing
    key k = do
        on <- testKey k
        if on
            then return (Just k)
            else key (k+1)
