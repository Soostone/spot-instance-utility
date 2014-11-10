{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Web.SIU.Tests.Analysis.Internal
    (analysisInternalTests
    ) where

-------------------------------------------------------------------------------
import           Control.Lens
import           System.Random
import           System.Random.Shuffle
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Web.SIU.Analysis.Internal
import           Web.SIU.Tests.Arbitrary   ()
-------------------------------------------------------------------------------

analysisInternalTests :: TestTree
analysisInternalTests = testGroup "Web.SIU.Analysis.Internal"
  [
    testProperty "avgStdev non negative" $ \xs -> snd (avgStdev xs) >= 0
  , testProperty "avgStdev order invariant" $ \(NonEmpty xs) (gen :: StdGen) ->
     avgStdev xs `closeEnough` avgStdev (shuffle' xs (length xs) gen)
  ]


-------------------------------------------------------------------------------
rounded :: Double -> Integer
rounded = truncate . (100000 *)


-------------------------------------------------------------------------------
closeEnough
  :: (Eq a, Show a, Each a1 a Double Integer) => a1 -> a1 -> Property
closeEnough a b = (a & each %~ rounded) === (b & each %~ rounded)
