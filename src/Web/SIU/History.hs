{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Web.SIU.History
    ( historyStream
    )where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.ByteString             (ByteString)
import           Data.Conduit
import           Data.Conduit.Process
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           System.Exit
-------------------------------------------------------------------------------
import           Web.SIU.Types
import           Web.SIU.Utils
-------------------------------------------------------------------------------

--TODO: remove
instance Show CmdSpec where
  show (RawCommand cmd args) = unwords (cmd:args)


historyStream :: (MonadIO m) => SIUOptions -> IO (Source m ByteString, Source m ByteString, IO ExitCode)
historyStream opts = do
    cp <- mkHistProc opts
    (ClosedStream, (childStdout, close), (childStderr, close'), cph) <- streamingProcess cp
    let closer = close >> close' >> waitForStreamingProcess cph
    return (childStdout, childStderr, closer)


-------------------------------------------------------------------------------
mkHistProc SIUOptions {..} = do
    now <- getCurrentTime
    let duration = fromMaybe [] (mkDurationFlags now <$> siuDuration)
    let args = staticFlags ++
               itypes ++
               azs ++
               duration ++
               [mkFlag "-d" $ show siuProductDescription]
    return $ CreateProcess { cmdspec       = RawCommand cmd args
                           , cwd           = Nothing
                           , env           = Nothing
                           , std_in        = Inherit
                           , std_out       = Inherit
                           , std_err       = Inherit
                           , close_fds     = False
                           , create_group  = False
                           }
  where
    cmd = "ec2-describe-spot-price-history"
    itypes = map (mkFlag "-t" . fst) $ M.toList siuInstanceTypes
    azs = map (mkFlag "-a") siuAvailabilityZones
    mkFlag f v = f ++ show v
    staticFlags = ["-H"]
    mkDurationFlags now d = [ mkFlag "-s" . fmtTime $ subtractDuration d now
                            , mkFlag "-e" . fmtTime $ now]


-------------------------------------------------------------------------------
subtractDuration d = addUTCTime diff
  where
    diff = -(durationSeconds d)


-------------------------------------------------------------------------------
durationSeconds (Days n) = fromIntegral $ n * 60 * 60 * 24
durationSeconds (Weeks n) = fromIntegral $ n * 60 * 60 * 24 * 7


-------------------------------------------------------------------------------
fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale timeFmt
