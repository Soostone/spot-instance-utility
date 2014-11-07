module Web.SIU.Utils
    ( showBS
    , timeFmt
    , defaultTimeLocale
    , invertMS
    , note
    , awsCSVSettings
    ) where


-------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import           Data.CSV.Conduit
import qualified Data.Map.Strict       as MS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Tuple
import           System.Locale         (defaultTimeLocale)
-------------------------------------------------------------------------------

-- | This takes the long route but in my testing, with BS.pack . show
-- you can get invalid UTF-8 sequences
showBS :: (Show a) => a -> ByteString
showBS = T.encodeUtf8 . T.pack . show


-------------------------------------------------------------------------------
timeFmt :: String
timeFmt = "%Y-%m-%dT%H:%M:%S%z"


-------------------------------------------------------------------------------
invertMS :: (Ord k, Ord v) => MS.Map k v -> MS.Map v k
invertMS = MS.fromList . map swap . MS.toList


-------------------------------------------------------------------------------
note :: b -> Maybe a -> Either b a
note _ (Just a) = Right a
note b Nothing  = Left b


-------------------------------------------------------------------------------
awsCSVSettings :: CSVSettings
awsCSVSettings = defCSVSettings { csvSep = '\t' }
