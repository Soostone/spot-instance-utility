{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Web.SIU.CLI
    ( opts
    , run
    ) where


-------------------------------------------------------------------------------
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch         (MonadThrow)
import           Data.ByteString             (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary         (sinkHandle)
import qualified Data.Conduit.List           as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Data.Monoid
import           Options.Applicative         as OA
import           System.IO
import           Text.Read
-------------------------------------------------------------------------------
import           Web.SIU.Analysis
import           Web.SIU.History
import           Web.SIU.Types
import           Web.SIU.Utils
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
run :: SIUOptions -> IO ()
run siuo = do
  streamTriples <- historyStream siuo
  let cout = mconcat $ streamTriples^..each._1
  let cerr = mconcat $ streamTriples^..each._2
  let closers = streamTriples^..each._3
  csvPipeline awsCSVSettings cout (CL.map getNamed =$= analyze siuo) (sinkHandle stdout)
  cerr $$ sinkHandle stderr
  sequence_ closers
  where


-------------------------------------------------------------------------------
csvPipeline
    :: ( ToNamedRecord b, MonadThrow m,
         CSV ByteString a, CSV ByteString (Row ByteString)
       , Show a, Show b) -- FIXME: spurious
    => CSVSettings
    -> Conduit () m ByteString
    -> Sink a m [b]
    -> Sink ByteString m ()
    -> m ()
csvPipeline set source c sink = do
  bs <- source $= intoCSV set $$ c
  CL.sourceList bs $= CL.map toNamedRecord $= (writeHeaders set >> fromCSV set) $$ sink


-------------------------------------------------------------------------------
opts :: ParserInfo SIUOptions
opts = info (helper <*> optParser)
            (fullDesc <> progDesc' <> header')
  where
    progDesc' = progDesc "Analyze spot instance history for the given INSTANCE_PAIRs"
    header' = header "spot-instance-utility - Tool for selecting EC2 spot instances based on historic prices"


-------------------------------------------------------------------------------
optParser :: OA.Parser SIUOptions
optParser = SIUOptions
    <$> optional (option auto
                  (long "duration" <>
                   short 'd' <>
                   metavar "DURATION" <>
                   help "Duration of days or weeks, such as 3d or 2w"))
    <*> (M.fromList
          <$> (map parseIP <$>
               many (strArgument (
                        metavar "INSTANCE_PAIR" <>
                        help ("Pair of instance type and optional count, defaulting to one. Instance types: " ++ itOpts)))))
    <*> many (option auto (long "region" <>
                           short 'r' <>
                           metavar "REGION" <>
                           help ("Region, which will be expanded into availability zones. Can be repated. You'll probably want to specify this, otherwise it it will default to us-east-1. Options: " ++ rOpts)))
    <*> option auto (long "product-description" <>
                     short 'p' <>
                     value LinuxUNIX <>
                     metavar "PRODUCT_DESCRIPTION" <>
                     help ("EC2 product description string. Options: " ++ pdOpts) <>
                     showDefault)
    <*> option auto (long "sigmas" <>
                     short 's' <>
                     value 3 <>
                     metavar "SIGMAS" <>
                     help "Stability is measured in number of times deviated > SIGMAS sigmas from the most frequent value." <>
                     showDefault)
  where
    pdOpts = showOptions $ M.elems pdOptions
    itOpts = showOptions $ M.elems itOptions
    rOpts = showOptions $ M.elems rOptions


-------------------------------------------------------------------------------
showOptions :: (Show a) => [a] -> String
showOptions = intercalate "|" . map show


-------------------------------------------------------------------------------
parseIP :: String -> (InstanceType,Int)
parseIP s = (read it, readDef 1 n)
  where
    (it,n) = breakOn ',' s
    readDef d = fromMaybe d . readMaybe


-------------------------------------------------------------------------------
breakOn :: Eq a => a -> [a] -> ([a], [a])
breakOn x = second (drop 1) . break (== x)
