module Main
    (main
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import           Web.SIU.Tests.Arbitrary
import           Web.SIU.Tests.Analysis.Internal
import           Web.SIU.Tests.Types
import           Web.SIU.Tests.Utils
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "spot-instance-utility"
  [ typesTests
  , utilsTests
  , analysisInternalTests
  ]

