module Web.SIU.Utils
    ( showBS
    , timeFmt
    , defaultTimeLocale
    ) where


-------------------------------------------------------------------------------
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.Locale         (defaultTimeLocale)
-------------------------------------------------------------------------------

-- | This takes the long route but in my testing, with BS.pack . show
-- you can get invalid UTF-8 sequences
showBS :: (Show a) => a -> ByteString
showBS = T.encodeUtf8 . T.pack . show


-------------------------------------------------------------------------------
timeFmt :: String
timeFmt = "%Y-%m-%dT%H:%M:%S%z"
