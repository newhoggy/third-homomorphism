{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Third.Homomorphism
  ( Tree(..)
  , Zipper
  , z2t
  , sizeTree
  , mappendTree
  , mappendTreeDn
  , mappendTreeUp
  ) where

data Tree a = Bin a (Tree a) (Tree a) | Tip
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Tip           = Tip
  fmap f (Bin a lt rt) = Bin (f a) (fmap f lt) (fmap f rt)

type Zipper a = [Either (a, Tree a) (a, Tree a)]

z2t :: Zipper a -> Tree a
z2t []                 = Tip
z2t (Left  (a, lt):rz) = Bin a  lt       (z2t rz)
z2t (Right (a, rt):lz) = Bin a (z2t lz)   rt

sizeTree :: Tree a -> Int
sizeTree Tip         = 0
sizeTree (Bin _ a b) = 1 + sizeTree a + sizeTree b

mappendTree :: Monoid a => Tree a -> a
mappendTree Tip           = mempty
mappendTree (Bin a lt rt) = a <> mappendTree lt <> mappendTree rt

mappendTreeDn :: Monoid a => Zipper a -> a
mappendTreeDn []                 = mempty
mappendTreeDn (Left  (a, lt):rz) = (mappendTree   lt <> a) <> mappendTreeDn rz
mappendTreeDn (Right (a, rt):lz) = (mappendTreeDn lz <> a) <> mappendTree   rt

mappendTreeUp :: Monoid a => Zipper a -> a
mappendTreeUp []                 = mempty
mappendTreeUp (Left  (a, lt):rz) = mappendTree   lt <> (a <> mappendTreeUp rz)
mappendTreeUp (Right (a, rt):lz) = mappendTreeUp lz <> (a <> mappendTree   rt)
