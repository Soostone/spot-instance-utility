{-# LANGUAGE TemplateHaskell #-}
module Web.SIU.Tests.Arbitrary where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Instances
-------------------------------------------------------------------------------
import           Web.SIU.Types
-------------------------------------------------------------------------------

$(derive makeArbitrary ''Duration)
$(derive makeArbitrary ''Region)
$(derive makeArbitrary ''AvailabilityZone)
$(derive makeArbitrary ''ProductDescription)
$(derive makeArbitrary ''InstanceType)
$(derive makeArbitrary ''SIUOfferingAnalysis)
$(derive makeArbitrary ''SpotPriceChange)

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary
