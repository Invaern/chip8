module VideoSpec (videoTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Emulator.Video as EV
import           Control.Monad.ST     (runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import Data.Bits
import Data.Word (Word8)

videoTests :: TestTree
videoTests = testGroup "Video sub system"
    [ 
      drawAlignedPoint
    , singlePointCollision
    , drawAlignedPointWithCollision
    , drawAlignedHorizontalWrapping
    , drawAlignedVerticalWrapping
    , drawNotAlignedPoint
    , drawNotAlignedPointWithCollision
    , prop_activePointsAfterDraw
    , initiallyClear
    ]

prop_activePointsAfterDraw :: TestTree
prop_activePointsAfterDraw = testProperty "Draw byte ==> popCount byte == length activePoints" $
    \x y val -> (getPoints x y val) == popCount val
  where
    getPoints :: Word8 -> Word8 -> Word8 -> Int
    getPoints x y val = runST $ do
      video <- EV.new
      EV.draw video x y val
      points <- EV.activePoints video
      return $ length points

drawAlignedPoint :: TestTree
drawAlignedPoint = testCase "Draw aligned point" $ do
    let expected = [(x,0) | x <- [0..7]]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "No collision expected" False collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        collision <- EV.draw video 0 0 0xFF
        points <- EV.activePoints video
        return (points, collision)

singlePointCollision :: TestTree
singlePointCollision = testCase "Single point collision" $ do
    let expected = []
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "Collision expected" True collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        EV.draw video 0 0 0x1
        collision <- EV.draw video 0 0 0x1
        points <- EV.activePoints video
        return (points, collision)

drawAlignedPointWithCollision :: TestTree
drawAlignedPointWithCollision = testCase "Draw aligned point with collision" $ do
    let expected = [(0,0), (1,0), (6,0), (7,0)]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "Collision expected" True collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        EV.draw video 0 0 0xFF
        collision <- EV.draw video 0 0 0x3C -- 0011 1100
        points <- EV.activePoints video
        return (points, collision)

drawNotAlignedPoint :: TestTree
drawNotAlignedPoint = testCase "Draw not aligned point" $ do
    let expected = [(6,0), (7,0), (9, 0)]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "No collision expected" False collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        collision <- EV.draw video 4 0 0x34 -- 0011 0100
        points <- EV.activePoints video
        return (points, collision)

drawNotAlignedPointWithCollision :: TestTree
drawNotAlignedPointWithCollision = testCase "Draw not aligned point with collision" $ do
    let expected = [(4,0), (9,0)]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "Collision expected" True collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        EV.draw video 4 0 0xFF
        collision <- EV.draw video 4 0 0x7B
        points <- EV.activePoints video
        return (points, collision)

drawAlignedHorizontalWrapping :: TestTree
drawAlignedHorizontalWrapping = testCase "Draw aligned point with horizontal wrapping" $ do
    let expected = [(x,0) | x <- [8..15]]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "No collision expected" False collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        collision <- EV.draw video 72 0 0xFF
        points <- EV.activePoints video
        return (points, collision)

drawAlignedVerticalWrapping :: TestTree
drawAlignedVerticalWrapping = testCase "Draw aligned point with vertical wrapping" $ do
    let expected = [(x,1) | x <- [0..7]]
    eqAnyOrder "Expected different active points" expected activePoints 
    assertEqual "No collision expected" False collision 
  where
    (activePoints, collision) = runST $ do
        video <- EV.new
        collision <- EV.draw video 0 33 0xFF
        points <- EV.activePoints video
        return (points, collision)

initiallyClear :: TestTree
initiallyClear = testCase "Initially no active points" $ do
    null activePoints @?= True
  where
    activePoints = runST $ do
        video <- EV.new
        points <- EV.activePoints video
        return points

eqAnyOrder :: (Eq a, Ord a, Show a) => String -> [a] -> [a] -> Assertion
eqAnyOrder msg expected actual = do
  assertEqual msg (L.sort expected) (L.sort actual)