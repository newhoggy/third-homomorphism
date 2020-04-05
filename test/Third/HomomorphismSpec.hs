module Third.HomomorphismSpec
  ( spec
  ) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen            as G
import qualified Hedgehog.Range          as R
import qualified Third.Homomorphism      as H
import qualified Third.Homomorphism.Fold as H
import qualified Third.Homomorphism.Gen  as G
import qualified Third.Homomorphism.Type as H

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "Third.HomomorphismSpec" $ do
  it "tree size" $ requireProperty $ do
    n <- forAll $ G.int (R.linear 0 10)
    t <- forAll $ G.treeSized n (G.int (R.linear 0 10))
    H.sizeTree t === n
  it "downward sum equals upward sum" $ requireProperty $ do
    t <- forAll $ fmap (fmap Sum) $ G.tree (R.linear 0 10) (G.int (R.linear 0 10))
    z <- forAll $ G.randomWalk t
    H.foldTreeDn z === H.foldTreeUp z
  it "downward weight equals upward weight" $ requireProperty $ do
    t <- forAll $ G.tree (R.linear 0 10) (G.int (R.linear 0 10))
    z <- forAll $ G.randomWalk t
    H.maxPathDn z === H.maxPathUp z
