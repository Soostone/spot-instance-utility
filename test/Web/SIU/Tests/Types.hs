{-# LANGUAGE RankNTypes #-}
module Web.SIU.Tests.Types
    ( typesTests
    ) where

-------------------------------------------------------------------------------
import           Control.Lens
import           Data.CSV.Conduit.Conversion
import           Data.Text                   (Text)
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Web.SIU.Tests.Arbitrary
import           Web.SIU.Types
-------------------------------------------------------------------------------

typesTests :: TestTree
typesTests = testGroup "Web.SIU.Types"
  [ testProperty "read/show Duration roundtrip" prop_show_read_Duration

  , testProperty "read/show AvailabilityZone roundtrip" prop_show_read_AvailabilityZone
  , testProperty "to/from named record AvailabilityZone roundtrip" prop_nr_AvailabilityZone

  , testProperty "read/show Region roundtrip" prop_show_read_Region
  , testProperty "text prism Region roundtrip" prop_text_prism_Region

  , testProperty "read/show InstanceType roundtrip" prop_show_read_InstanceType
  , testProperty "text prism InstanceType roundtrip" prop_text_prism_InstanceType

  , testProperty "read/show ProductDescription roundtrip" prop_show_read_ProductDescription
  , testProperty "text prism ProductDescription roundtrip" prop_text_prism_ProductDescription

  , testProperty "to/from named record SIUOfferingAnalysis roundtrip" prop_nr_SIUOfferingAnalysis
  , testProperty "to/from named record SpotPriceChange roundtrip" prop_nr_SpotPriceChange
  ]


-------------------------------------------------------------------------------
prop_show_read_Duration :: Duration -> Property
prop_show_read_Duration = prop_show_read


-------------------------------------------------------------------------------
prop_show_read_AvailabilityZone :: AvailabilityZone -> Property
prop_show_read_AvailabilityZone = prop_show_read


-------------------------------------------------------------------------------
prop_show_read_Region :: Region -> Property
prop_show_read_Region = prop_show_read


-------------------------------------------------------------------------------
prop_show_read_InstanceType :: InstanceType -> Property
prop_show_read_InstanceType = prop_show_read


-------------------------------------------------------------------------------
prop_show_read_ProductDescription :: ProductDescription -> Property
prop_show_read_ProductDescription = prop_show_read


-------------------------------------------------------------------------------
prop_show_read a = a === a'
  where
    a' = read $ show a


-------------------------------------------------------------------------------
prop_text_prism_Region :: Region -> Text -> Property
prop_text_prism_Region = prop_prism arText


-------------------------------------------------------------------------------
prop_text_prism_InstanceType :: InstanceType -> Text -> Property
prop_text_prism_InstanceType = prop_prism itText

-------------------------------------------------------------------------------
prop_text_prism_ProductDescription :: ProductDescription -> Text -> Property
prop_text_prism_ProductDescription = prop_prism pdText


-------------------------------------------------------------------------------
prop_prism :: (Eq a, Show a) => Prism' b a -> a -> b -> Property
prop_prism p a b = preview p (review p a) === Just a .&&. lengthOf p b <= 1


-------------------------------------------------------------------------------
prop_nr_AvailabilityZone :: AvailabilityZone -> Property
prop_nr_AvailabilityZone = prop_nr


-------------------------------------------------------------------------------
prop_nr_SIUOfferingAnalysis :: SIUOfferingAnalysis -> Property
prop_nr_SIUOfferingAnalysis = prop_nr


-------------------------------------------------------------------------------
prop_nr_SpotPriceChange :: SpotPriceChange -> Property
prop_nr_SpotPriceChange = prop_nr


-------------------------------------------------------------------------------
prop_nr a = Right a === a'
  where
    a' = runParser $ parseNamedRecord $ toNamedRecord a
