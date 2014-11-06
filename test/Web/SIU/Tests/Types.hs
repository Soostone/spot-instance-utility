module Web.SIU.Tests.Types
    ( typesTests
    ) where

-------------------------------------------------------------------------------
import           Data.CSV.Conduit.Conversion
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Web.SIU.Tests.Arbitrary
import           Web.SIU.Types
-------------------------------------------------------------------------------

typesTests :: TestTree
typesTests = testGroup "Web.SIU.Types"
  [ testProperty "read/show Duration roundtrip" prop_show_read_Duration
  , testProperty "read/show Availability roundtrip" prop_show_read_AvailabilityZone
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
prop_show_read a = a === a'
  where
    a' = read $ show a


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
