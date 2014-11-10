{-# LANGUAGE RecordWildCards #-}
module Web.SIU.Analysis.Internal where

-------------------------------------------------------------------------------
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
-------------------------------------------------------------------------------
import           Web.SIU.Types
import           Web.SIU.Utils
-------------------------------------------------------------------------------


type Acc = Map (InstanceType, AvailabilityZone) [Money]

-------------------------------------------------------------------------------
mkAnalysis :: SIUOptions -> Acc -> [SIUOfferingAnalysis]
mkAnalysis opts = sortBy cmp . mapMaybe (uncurry $ analyze1 opts) . M.toList
  where
    cmp = comparing _oaAverageCost <> comparing _oaDeviations


-------------------------------------------------------------------------------
analyze1 :: SIUOptions -> (InstanceType, AvailabilityZone) -> [Money] -> Maybe SIUOfferingAnalysis
analyze1 _ (_, _) [] = Nothing
analyze1 SIUOptions {..} (it, az) ms = Just SIUOfferingAnalysis {
      _oaInstanceType = it
    , _oaAvailabilityZone = az
    , _oaAverageCost = avg
    , _oaDeviations = deviationsCount
    }
  where
    (avg, stdev) = avgStdev ms
    mostFrequent = fst $ maximumBy (comparing snd) $ countUniques ms
    outlier m = diff m mostFrequent > fromIntegral _siuSigmas * stdev
    diff a b = abs $ a - b
    deviationsCount = count outlier ms


-------------------------------------------------------------------------------
avgStdev :: [Double] -> (Double, Double)
avgStdev [] = (0, 0)
avgStdev [x] = (x, 0)
avgStdev xs = (avg, stdev)
  where
    stdev = sqrt ((sum [(x - avg) ** 2 | x <- xs]) / fromIntegral (n - 1))
    avg = sum xs / fromIntegral (length xs)
    n = length xs
