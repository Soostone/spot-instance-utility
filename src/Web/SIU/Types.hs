{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Web.SIU.Types where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       as BS
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import qualified Data.Map.Lazy               as ML
import           Data.Map.Strict             (Map)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           GHC.Generics
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
newtype InstanceType = InstanceType {
     unIT :: Text
   } deriving (Eq,Ord,FromField)

instance Show InstanceType where
  show = T.unpack . unIT

instance Read InstanceType where
  readsPrec _ s = [(InstanceType $ T.pack s, "")]


-------------------------------------------------------------------------------
newtype AvailabilityZone = AvailabilityZone {
     unAZ :: Text
   } deriving (Eq,Ord,FromField)

instance Show AvailabilityZone where
  show = T.unpack . unAZ

instance Read AvailabilityZone where
  readsPrec _ s = [(AvailabilityZone $ T.pack s, "")]


-------------------------------------------------------------------------------
newtype ProductDescription = ProductDescription {
      unPD :: Text
    } deriving (Eq,FromField)

instance Show ProductDescription where
  show = T.unpack . unPD

instance Read ProductDescription where
  readsPrec _ s = [(ProductDescription $ T.pack s, "")]


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
    , siuInstanceTypes      :: Map InstanceType Int
    , siuAvailabilityZones  :: [AvailabilityZone]
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
