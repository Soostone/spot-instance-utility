{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Web.SIU.Types where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Char8       as BS
import           Data.CSV.Conduit.Conversion
import qualified Data.Map.Lazy               as ML
import qualified Data.Map.Strict             as MS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Lens
import           Data.Time
import           GHC.Generics                (Generic)
import           Text.Read
-------------------------------------------------------------------------------
import           Web.SIU.Utils
-------------------------------------------------------------------------------

type Money = Double


-------------------------------------------------------------------------------
newtype UTC = UTC { unUTC :: UTCTime }

instance FromField UTC where
  parseField s =
    maybe (fail "invalid UTC format") return $
      UTC <$> parseTime defaultTimeLocale timeFmt (BS.unpack s)

-------------------------------------------------------------------------------
data SpotPriceChange = SpotPriceChange {
      spcProductDescription :: ProductDescription
    , spcInstanceType       :: InstanceType
    , spcSpotPrice          :: Money
    , spcAvailabilityZone   :: AvailabilityZone
    } deriving (Show,Eq)


instance ToNamedRecord SpotPriceChange where
  toNamedRecord SpotPriceChange {..} = ML.fromList
    [ ("ProductDescription", showBS spcProductDescription)
    , ("InstanceType", showBS spcInstanceType)
    , ("Price", showBS spcSpotPrice)
    , ("AvailabilityZone", showBS spcAvailabilityZone)
    ]


instance FromNamedRecord SpotPriceChange where
  parseNamedRecord nr = SpotPriceChange
    <$> nr .: "ProductDescription"
    <*> nr .: "InstanceType"
    <*> nr .: "Price"
    <*> nr .: "AvailabilityZone"

-------------------------------------------------------------------------------
data InstanceType =
     T1Micro
   | M1Small
   | M1Medium
   | M1Large
   | M1Xlarge
   | M3Xlarge
   | M32xlarge
   | C1Medium
   | C1Xlarge
   | C34xlarge
   | C38xlarge
   | Cc14xlarge
   | Cc28xlarge
   | Cg14xlarge
   | Cr18xlarge
   | G22xlarge
   | M2Xlarge
   | M22xlarge
   | M24xlarge
   deriving (Eq,Ord)

instance FromField InstanceType where
  parseField = textPrismParseField itText "InstanceType"


itText :: Prism' Text InstanceType
itText = mapPrism itOptions

itOptions :: MS.Map Text InstanceType
itOptions = MS.fromList
  [ ("t1.micro", T1Micro)
  , ("m1.small", M1Small)
  , ("m1.medium", M1Medium)
  , ("m1.large", M1Large)
  , ("m1.xlarge", M1Xlarge)
  , ("m3.xlarge", M3Xlarge)
  , ("m3.2xlarge", M32xlarge)
  , ("c1.medium", C1Medium)
  , ("c1.xlarge", C1Xlarge)
  , ("c3.4xlarge", C34xlarge)
  , ("c3.8xlarge", C38xlarge)
  , ("cc1.4xlarge", Cc14xlarge)
  , ("cc2.8xlarge", Cc28xlarge)
  , ("cg1.4xlarge", Cg14xlarge)
  , ("cr1.8xlarge", Cr18xlarge)
  , ("g2.2xlarge", G22xlarge)
  , ("m2.xlarge", M2Xlarge)
  , ("m2.2xlarge", M22xlarge)
  , ("m2.4xlarge", M24xlarge)
  ]


instance Show InstanceType where
  show = textPrismShow itText

instance Read InstanceType where
  readsPrec = textPrismReadsPrec itText


-------------------------------------------------------------------------------
newtype AvailabilityZone = AvailabilityZone {
     unAZ :: Text
   } deriving (Eq,Ord,FromField)


instance Show AvailabilityZone where
  show = T.unpack . unAZ

instance Read AvailabilityZone where
  readsPrec _ s = [(AvailabilityZone $ T.pack s, "")]

instance FromNamedRecord AvailabilityZone where
  parseNamedRecord nr = AvailabilityZone <$> nr .: "Name"

instance ToNamedRecord AvailabilityZone where
  toNamedRecord az = ML.fromList
    [ ("Name", showBS az )
    ]


-------------------------------------------------------------------------------
data Region =
     ApNorthEast1
   | ApSouthEast1
   | ApSouthEast2
   | ApCentral1
   | EuWest1
   | SaEast1
   | UsEast1
   | UsWest1
   | UsWest2 deriving (Eq,Ord)

instance FromField Region where
  parseField = textPrismParseField arText "AvailiabilityRegion"


arText :: Prism' Text Region
arText = mapPrism rOptions

rOptions :: MS.Map Text Region
rOptions = MS.fromList
    [ ("ap-northeast-1", ApNorthEast1)
    , ("ap-southeast-1", ApSouthEast1)
    , ("ap-southeast-2", ApSouthEast2)
    , ("ap-central-1", ApCentral1)
    , ("eu-west-1", EuWest1)
    , ("sa-east-1", SaEast1)
    , ("us-east-1", UsEast1)
    , ("us-west-1", UsWest1)
    , ("us-west-2", UsWest2)
    ]

instance Show Region where
  show = textPrismShow arText

instance Read Region where
  readsPrec = textPrismReadsPrec arText


-------------------------------------------------------------------------------
data ProductDescription =
     LinuxUNIX
   | SUSELinux
   | Windows
   | LinuxUNIXAmazonVPC
   | SUSELinuxAmazonVPC
   | WindowsAmazonVPC
   deriving (Eq,Ord)

instance FromField ProductDescription where
  parseField = textPrismParseField pdText "ProductDescription"


pdText :: Prism' Text ProductDescription
pdText = mapPrism pdOptions


pdOptions :: MS.Map Text ProductDescription
pdOptions = MS.fromList
    [ ("Linux/UNIX", LinuxUNIX)
    , ("SUSE Linux", SUSELinux)
    , ("Windows", Windows)
    , ("Linux/UNIX (Amazon VPC)", LinuxUNIXAmazonVPC)
    , ("SUSE Linux (Amazon VPC)", SUSELinuxAmazonVPC)
    , ("Windows (Amazon VPC)", WindowsAmazonVPC)
    ]

instance Show ProductDescription where
  show = textPrismShow pdText

instance Read ProductDescription where
  readsPrec = textPrismReadsPrec pdText


-------------------------------------------------------------------------------
data Duration =
      Days Int
    | Weeks Int deriving (Eq)

instance Show Duration where
  show (Days i) = show i ++ "d"
  show (Weeks i) = show i ++ "w"


instance Read Duration where
  readPrec = do
    n <- readPrec
    rest <- getStr
    case rest of
      "d" -> return $ Days n
      "w" -> return $ Weeks n
      _   -> mzero


-------------------------------------------------------------------------------
getStr :: ReadPrec String
getStr = many get


-------------------------------------------------------------------------------
--TODO: shortcircuit empty instance types
--TODO: should duration be optional?
data SIUOptions = SIUOptions {
      siuDuration           :: Maybe Duration
    , siuInstanceTypes      :: MS.Map InstanceType Int
    , siuRegions            :: [Region]
    , siuProductDescription :: ProductDescription
    , siuSigmas             :: Int
    -- ^ empty list means all of them
    } deriving (Show,Eq)


-------------------------------------------------------------------------------
data SIUOfferingAnalysis = SIUOfferingAnalysis {
      oaInstanceType     :: InstanceType
    , oaAvailabilityZone :: AvailabilityZone
    , oaAverageCost      :: Money
    , oaDeviations       :: Int
    } deriving (Show,Eq,Generic)


instance FromNamedRecord SIUOfferingAnalysis where
  parseNamedRecord nr = SIUOfferingAnalysis
    <$> nr .: "InstanceType"
    <*> nr .: "AvailabilityZone"
    <*> nr .: "AverageCost"
    <*> nr .: "TimesDeviated"


instance ToNamedRecord SIUOfferingAnalysis where
  toNamedRecord SIUOfferingAnalysis {..} = ML.fromList
    [ ("InstanceType", showBS oaInstanceType)
    , ("AvailabilityZone", showBS oaAvailabilityZone)
    , ("AverageCost", showBS oaAverageCost)
    , ("TimesDeviated", showBS oaDeviations)
    ]


-------------------------------------------------------------------------------
-- | Boilerplate Reduction
-------------------------------------------------------------------------------
mapPrism :: (Ord k, Ord v) => MS.Map k v -> Prism' k v
mapPrism m = prism toKey fromKey
  where
    toKey v = invertMS m MS.! v
    fromKey k = note k $ MS.lookup k m


-------------------------------------------------------------------------------
textPrismShow :: Prism' Text v -> v -> String
textPrismShow p = T.unpack . review p


-------------------------------------------------------------------------------
textPrismReadsPrec :: Prism' Text v -> Int -> ReadS v
textPrismReadsPrec p _ s =  maybe [] (\x -> [(x, "")]) mVal
  where
    mVal = s ^. packed ^? p


-------------------------------------------------------------------------------
textPrismParseField :: Prism' Text b -> String -> ByteString -> Parser b
textPrismParseField p typ f = convert =<< parseField f
  where
    convert t = maybe (fail $ "invalid " ++ typ ++ ": " ++ show t)
                      return
                      (t ^? p)
