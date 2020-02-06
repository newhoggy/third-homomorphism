module Third.Homomorphism
  ( Tree(..)
  , Zipper
  , z2t
  ) where

data Tree a = Bin a (Tree a) (Tree a) | Tip
  deriving (Eq, Show)

type Zipper a = [Either (a, Tree a) (a, Tree a)]

z2t :: Zipper a -> Tree a
z2t []                 = Tip
z2t (Left  (a, lt):rz) = Bin a  lt       (z2t rz)
z2t (Right (a, rt):lz) = Bin a (z2t lz)   rt
