{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes      #-}
module Web.SIU.Analysis
    ( analyze
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
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


--TODO: cleaner with lenses
analyze :: (Monad m) => SIUOptions -> Consumer SpotPriceChange m [SIUOfferingAnalysis]
--TODO: remove
analyze siuo@SIUOptions {..} = CL.filter matchesAZ =$=
                               CL.map multPrice =$=
                               (mkAnalysis siuo <$> CL.fold updateAcc mempty)
  where
    matchesAZ SpotPriceChange{..}
      | null siuAvailabilityZones = True
      | otherwise                 = spcAvailabilityZone `elem` siuAvailabilityZones
    multPrice spc = spc { spcSpotPrice = findMult spc * spcSpotPrice spc}
    findMult :: SpotPriceChange -> Money
    findMult spc = fromIntegral $ fromMaybe 1 $ M.lookup (spcInstanceType spc) siuInstanceTypes
    updateAcc :: Acc -> SpotPriceChange -> Acc
    updateAcc acc SpotPriceChange {..} = M.insertWith mappend (spcInstanceType, spcAvailabilityZone) [spcSpotPrice] acc

type Acc = Map (InstanceType, AvailabilityZone) [Money]

-------------------------------------------------------------------------------
mkAnalysis :: SIUOptions -> Acc -> [SIUOfferingAnalysis]
mkAnalysis opts = sortBy cmp . mapMaybe (uncurry $ analyze1 opts) . M.toList
  where
    cmp = comparing oaAverageCost <> comparing oaDeviations


-------------------------------------------------------------------------------
analyze1 :: SIUOptions -> (InstanceType, AvailabilityZone) -> [Money] -> Maybe SIUOfferingAnalysis
analyze1 _ (_, _) [] = Nothing
analyze1 SIUOptions {..} (it, az) ms = Just SIUOfferingAnalysis {
      oaInstanceType = it
    , oaAvailabilityZone = az
    , oaAverageCost = avg
    , oaDeviations = deviationsCount
    }
  where
    (avg, stdev) = avgStdev ms
    mostFrequent = fst $ maximumBy (comparing snd) $ countUniques ms
    outlier m = diff m mostFrequent > fromIntegral siuSigmas * stdev
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
