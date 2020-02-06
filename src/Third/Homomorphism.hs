module Third.Homomorphism
  ( Tree(..)
  , Zipper
  , z2t
  , mappendTree
  , mappendTreeDn
  , mappendTreeUp
  ) where

data Tree a = Bin a (Tree a) (Tree a) | Tip
  deriving (Eq, Show)

type Zipper a = [Either (a, Tree a) (a, Tree a)]

z2t :: Zipper a -> Tree a
z2t []                 = Tip
z2t (Left  (a, lt):rz) = Bin a  lt       (z2t rz)
z2t (Right (a, rt):lz) = Bin a (z2t lz)   rt

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
