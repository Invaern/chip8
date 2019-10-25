{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Emulator.Monad.ST
    ( STEmulator
    , runSTEmulator
    ) where

import           Control.Monad.Reader (ReaderT, ask, runReaderT, reader)
import           Control.Monad.ST     (ST, runST)
import           Control.Monad.Trans  (lift)

import           Emulator.CPU      (CPU)
import qualified Emulator.CPU      as CPU
import qualified Emulator.Memory      as Memory
import           Emulator.Monad

newtype STEmulator s a = STEmulator (ReaderT (CPU s) (ST s) a) deriving (Functor, Applicative, Monad)

-- instance MonadEmulator (STEmulator s) where
--     load address = STEmulator $ do
--         mem <- reader CPU.memory
--         lift $ Memory.load mem address

--     store val address = STEmulator $ do
--         mem <- reader CPU.memory
--         lift $ Memory.store mem val address


runSTEmulator :: (forall s. STEmulator s a) -> a
runSTEmulator emu = undefined
-- runSTEmulator emu =
--     runST $ run emu
--   where
--     run (STEmulator reader) = do
--         cpu <- CPU.new
--         runReaderT reader cpu

