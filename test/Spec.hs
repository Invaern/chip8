import Test.Tasty

import VideoSpec (videoTests)
import KeyboardSpec (keyboardTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
    [
      videoTests
    , keyboardTests
    ]

