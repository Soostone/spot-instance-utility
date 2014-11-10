{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Web.SIU.Analysis
    ( analyze
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List   as CL
import           Data.List
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
-------------------------------------------------------------------------------
import           Web.SIU.Types
-------------------------------------------------------------------------------


analyze :: (Monad m) => SIUOptions -> Consumer SpotPriceChange m [SIUOfferingAnalysis]
analyze siuo@SIUOptions {..} = CL.map multPrice =$=
                               (mkAnalysis siuo <$> CL.fold updateAcc mempty)
  where
    multPrice spc = spc & spcSpotPrice *~ findMult spc
    findMult :: SpotPriceChange -> Money
    findMult SpotPriceChange{..} = _siuInstanceTypes ^. at _spcInstanceType .
                                                        non 1 .
                                                        to fromIntegral
    updateAcc :: Acc -> SpotPriceChange -> Acc
    updateAcc acc SpotPriceChange {..} = acc & at (_spcInstanceType, _spcAvailabilityZone)
                                             <>~ Just [_spcSpotPrice]

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
countUniques :: (Eq a, Ord a) => [a] -> [(a, Int)]
countUniques l = M.toList $ M.fromListWith (+) $ zip l (repeat 1)


-------------------------------------------------------------------------------
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


-------------------------------------------------------------------------------
avgStdev :: [Double] -> (Double, Double)
avgStdev [] = (0, 0)
avgStdev xs = (avg, stdev)
  where
    stdev = sqrt ((sum [(x - avg) ** 2 | x <- xs]) / fromIntegral (n - 1))
    avg = sum xs / fromIntegral (length xs)
    n = length xs
