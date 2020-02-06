{-# LANGUAGE MultiWayIf #-}

module Third.Homomorphism.Gen
  ( tree
  , treeSized
  , randomWalk
  ) where

import Hedgehog
import Third.Homomorphism.Type

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

tree :: MonadGen m => Range Int -> m a -> m (Tree a)
tree r g = do
  n <- G.int r
  treeSized n g

treeSized :: MonadGen m => Int -> m a -> m (Tree a)
treeSized n g = if
  | n == 0    -> pure Tip
  | n > 0     -> do
      x <- g
      i <- G.int (R.linear 0 (n - 1))
      a <- treeSized          i  g
      b <- treeSized (n - 1 - i) g
      return (Bin x a b)
  | otherwise -> error "Invalid tree size"

randomWalk :: MonadGen m => Tree a -> m (Zipper a)
randomWalk Tip = pure []
randomWalk (Bin x lt rt) = do
  chooseLeft <- G.bool
  if chooseLeft
    then (Left (x, rt):) <$> randomWalk lt
    else (Left (x, lt):) <$> randomWalk rt
