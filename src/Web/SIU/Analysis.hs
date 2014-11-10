{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Web.SIU.Analysis
    ( analyze
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.Monoid
-------------------------------------------------------------------------------
import           Web.SIU.Analysis.Internal
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
