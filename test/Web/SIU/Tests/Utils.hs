{-# LANGUAGE ScopedTypeVariables #-}
module Web.SIU.Tests.Utils
    ( utilsTests
    ) where

-------------------------------------------------------------------------------
import           Data.List
import qualified Data.Set              as S
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Web.SIU.Utils
-------------------------------------------------------------------------------

utilsTests :: TestTree
utilsTests = testGroup "Web.SIU.Utils"
  [ testProperty "count empty" $ \(Blind pred) ->
      count pred ([] :: [()]) === 0
  , testProperty "count partition" $ \(Blind pred) (xs :: [Int]) ->
      count pred xs === length (fst (partition pred xs))

  , testProperty "countUniques empty" $ countUniques ([] :: [()]) === []
  , testProperty "countUniques size" $ \(xs :: [Int]) ->
     length (countUniques xs) === S.size (S.fromList xs)
  , testProperty "countUniques counts" $ \(xs :: [Int]) ->
     sum (map snd (countUniques xs)) === length xs
  , testProperty "countUniques values" $ \(xs :: [Int]) ->
     let vals = sort $ map fst $ countUniques xs
     in vals === sort (nub xs)
  ]
