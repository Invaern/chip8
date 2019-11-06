module KeyboardSpec (keyboardTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import           Control.Monad.ST     (runST)
import Control.Monad (forM, forM_)
import qualified Data.List as L

import qualified Emulator.Keyboard as EK

keyboardTests :: TestTree
keyboardTests = testGroup "Keyboard"
    [
      initiallyAllOff
    , only16KeysCanBeSet
    , setOnlyOneKey
    ]

initiallyAllOff :: TestTree
initiallyAllOff = testCase "Initially all off" $ do
    assertBool "All off" $ (not . or) keys
  where
    keys = runST $ do
        keyboard <- EK.new
        forM [0..0xFF] $ \key -> do
            EK.isOn keyboard key

only16KeysCanBeSet :: TestTree
only16KeysCanBeSet = testCase "Only 0x00-0x0F range can be set" $ do
    let expectedOn = take 16 (repeat True)
        expectedOff = take (0xFF - 0x0F) (repeat False)
    assertEqual "All on" expectedOn (take 16 keys)
    assertEqual "All off" expectedOff (drop 16 keys)
  where
    keys = runST $ do
        keyboard <- EK.new
        forM [0..0xFF] $ \key -> do
            EK.set keyboard key
            EK.isOn keyboard key

setOnlyOneKey :: TestTree
setOnlyOneKey = testCase "Only one key should be set" $ do
  forM_ [0..0x0F] $ \key -> do
      let result = withKey key
          setKeys = L.findIndices id result
      assertEqual "Expected set keys" [fromIntegral key] setKeys

  where
    withKey key = runST $ do
      keyboard <- EK.new
      EK.set keyboard key
      mapM (EK.isOn keyboard) [0..0x0F]