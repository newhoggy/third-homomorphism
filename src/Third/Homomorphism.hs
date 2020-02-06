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
  , maxPath
  , maxPathDn
  , maxPathUp
  , maxPathRi
  , maxPathRiAppend
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

maxPath :: Tree Int -> Int
maxPath Tip           = 0
maxPath (Bin n lt rt) = n + max (maxPath lt) (maxPath rt)

maxPathDn :: Zipper Int -> (Int, Int)
maxPathDn = dn . reverse
  where dn :: Zipper Int -> (Int, Int)
        dn []                 = (0, 0)
        dn (Left  (n, lt):rz) = let (m, w) = dn rz in (max m (w + n + maxPath lt), w + n)
        dn (Right (n, rt):lz) = let (m, w) = dn lz in (max m (w + n + maxPath rt), w + n)

maxPathUp :: Zipper Int -> (Int, Int)
maxPathUp []                 = (0, 0)
maxPathUp (Left  (n, lt):rz) = let (m, w) = maxPathUp rz in (n + max m (maxPath lt), n + w)
maxPathUp (Right (n, rt):lz) = let (m, w) = maxPathUp lz in (n + max m (maxPath rt), n + w)

maxPathRi :: (Int, Int) -> [Either (Int, Tree Int)  (Int, Tree Int)]
maxPathRi (m, w) = [Left (w, Bin (m - w) Tip Tip)]

maxPathRiAppend :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxPathRiAppend (m1, w1) (m2, w2) = maxPathDn (maxPathRi (m1, w1) ++ maxPathRi (m2, w2))

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
