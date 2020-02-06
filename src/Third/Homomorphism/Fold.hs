module Third.Homomorphism.Fold
  ( foldTree
  , foldTreeDn
  , foldTreeUp
  ) where

import Third.Homomorphism.Type

foldTree :: Monoid a => Tree a -> a
foldTree Tip           = mempty
foldTree (Bin a lt rt) = a <> foldTree lt <> foldTree rt

foldTreeDn :: Monoid a => Zipper a -> a
foldTreeDn []                 = mempty
foldTreeDn (Left  (a, lt):rz) = (foldTree   lt <> a) <> foldTreeDn rz
foldTreeDn (Right (a, rt):lz) = (foldTreeDn lz <> a) <> foldTree   rt

foldTreeUp :: Monoid a => Zipper a -> a
foldTreeUp []                 = mempty
foldTreeUp (Left  (a, lt):rz) = foldTree   lt <> (a <> foldTreeUp rz)
foldTreeUp (Right (a, rt):lz) = foldTreeUp lz <> (a <> foldTree   rt)
