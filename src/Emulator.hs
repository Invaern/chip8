{-# LANGUAGE BangPatterns #-}
module Emulator where


import           Control.Monad        (replicateM_, unless, when)
import           Data.Bits
import           Data.Word            (Word8)
import           Emulator.CPU         (Timer (..))
import           Emulator.Instruction (Instruction(..), decodeInstruction)
import           Emulator.Monad       (MonadEmulator (..), System (..))

import Debug.Trace

step :: (MonadEmulator m) => m ()
step = do
    val <- load 0
    store (val + 1) 0

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
            --step
            -- stepTime <- currentMilis  
            -- traceM ("step time: " ++ show stepTime)
            render
            !endTime <- currentMilis
            -- traceM ("end time: " ++ show endTime)
            updateTimers startTime endTime
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

updateTimers :: (MonadEmulator m) => Double -> Double -> m ()
updateTimers startTime endTime = do
    updateTimers' $ elapsedW8 (endTime - startTime)
  where
    elapsedW8 :: Double -> Word8
    elapsedW8 elapsed | elapsed > 255 = 255
                      | otherwise = floor elapsed


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


updateTimers' :: MonadEmulator m => Word8 -> m ()
updateTimers' elapsed = do
    delayT <- readTimer DelayTimer
    soundT <- readTimer SoundTimer
    when (delayT > 0) $ writeTimer DelayTimer (sub delayT elapsed)
    when (soundT > 0) $ writeTimer SoundTimer (sub soundT elapsed)
  where
    sub :: Word8 -> Word8 -> Word8
    sub a b | a >= b    = a - b
            | otherwise = 0


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
executeInstruction x = undefined
