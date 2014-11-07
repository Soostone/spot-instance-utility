{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Web.SIU.History
    ( historyStream
    )where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List           as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           System.Exit
import           System.IO
import           System.Process
-------------------------------------------------------------------------------
import           Web.SIU.Types
import           Web.SIU.Utils
-------------------------------------------------------------------------------

--TODO: remove
instance Show CmdSpec where
  show (RawCommand cmd args) = unwords (cmd:args)


-- these SHENANIGANS are because conduit process and the aws tools
-- don't stream contents if the output is beyond a certain size
historyStream :: SIUOptions -> IO (Source IO ByteString, Source IO ByteString, IO ExitCode)
historyStream = uncurry slurpCommand <=< mkHistProc


-------------------------------------------------------------------------------
-- | Call ec2-describe-availability-zones. Unfortunately this command
-- only supports one region at a time so it will take repeated calls
-- to expand multiple regions
getAvailabilityZones :: Region -> IO [AvailabilityZone]
getAvailabilityZones r = do
  (out, err, cph) <- slurpCommand "ec2-describe-availability-zones" args
  err $$ sinkHandle stderr
  (out $= intoCSV awsCSVSettings $= CL.map getNamed $$ CL.consume ) `finally` cph
  where
    args = ["-H", "--region=" ++ show r]

-------------------------------------------------------------------------------
slurpCommand :: String -> [String] -> IO (Source IO ByteString, Source IO ByteString, IO ExitCode)
slurpCommand cmd args = do
    print $ cmdspec cp
    (_, Just childStdout, Just childStderr, cph) <- createProcess cp
    hSetBinaryMode childStdout True
    hSetBinaryMode childStderr True
    rawOut <- LBS.hGetContents childStdout
    rawErr <- LBS.hGetContents childStderr
    return (sourceLbs rawOut, sourceLbs rawErr, waitForProcess cph)
  where
    cp = CreateProcess { cmdspec       = RawCommand cmd args
                       , cwd           = Nothing
                       , env           = Nothing
                       , std_in        = Inherit
                       , std_out       = CreatePipe
                       , std_err       = CreatePipe
                       , close_fds     = True
                       , create_group  = False
                       }

-------------------------------------------------------------------------------
mkHistProc :: SIUOptions -> IO (String, [String])
mkHistProc SIUOptions {..} = do
    now <- getCurrentTime
    finalAzs <- concat <$> mapM getAvailabilityZones siuRegions
    let duration = fromMaybe [] (mkDurationFlags now <$> siuDuration)
    let args = staticFlags ++
               itypes ++
               azs finalAzs ++
               duration ++
               [mkFlag "-d" $ show siuProductDescription]
    return (cmd, args)
  where
    cmd = "ec2-describe-spot-price-history"
    itypes = map (mkFlag "-t" . fst) $ M.toList siuInstanceTypes
    azs = map (mkFlag "-a")
    mkFlag f v = f ++ show v
    staticFlags = ["-H"]
    mkDurationFlags now d = [ mkFlag "-s" . fmtTime $ subtractDuration d now
                            , mkFlag "-e" . fmtTime $ now]


-------------------------------------------------------------------------------
subtractDuration :: Duration -> UTCTime -> UTCTime
subtractDuration d = addUTCTime diff
  where
    diff = -(durationSeconds d)


-------------------------------------------------------------------------------
durationSeconds :: Duration -> NominalDiffTime
durationSeconds (Days n) = fromIntegral $ n * 60 * 60 * 24
durationSeconds (Weeks n) = fromIntegral $ n * 60 * 60 * 24 * 7


-------------------------------------------------------------------------------
fmtTime :: UTCTime -> String
fmtTime = formatTime defaultTimeLocale timeFmt
