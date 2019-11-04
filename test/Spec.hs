import Test.Tasty

import VideoSpec (videoTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [videoTests]

