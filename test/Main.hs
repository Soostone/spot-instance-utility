module Main
    (main
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import           Web.SIU.Tests.Arbitrary
import           Web.SIU.Tests.Types
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "spot-instance-utility"
  [ typesTests
  ]

