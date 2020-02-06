{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Third.Homomorphism.Type
  ( Tree(..)
  , Zipper
  , sizeTree
  , z2t
  , t2zLt
  , t2zRt
  , t2zAlt
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

t2zLt :: Tree a -> Zipper a
t2zLt (Bin a lt rt) = Left (a, rt):t2zLt lt
t2zLt Tip           = []

t2zRt :: Tree a -> Zipper a
t2zRt (Bin a lt rt) = Right (a, lt):t2zRt rt
t2zRt Tip           = []

t2zAlt :: Tree a -> Zipper a
t2zAlt = goL
  where goL :: Tree a -> Zipper a
        goL (Bin a lt rt) = Left  (a, rt):goR lt
        goL Tip           = []
        goR :: Tree a -> Zipper a
        goR (Bin a lt rt) = Right (a, lt):goL rt
        goR Tip           = []

sizeTree :: Tree a -> Int
sizeTree Tip         = 0
sizeTree (Bin _ a b) = 1 + sizeTree a + sizeTree b
