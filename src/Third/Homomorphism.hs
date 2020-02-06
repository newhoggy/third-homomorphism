module Third.Homomorphism
  ( mappendTree
  , mappendTreeDn
  , mappendTreeUp
  , maxPath
  , maxPathDn
  , maxPathUp
  , maxPathRi
  , maxPathRiAppend
  , maxPathPar
  ) where

import Third.Homomorphism.Type

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

maxPathPar :: Zipper Int -> Int
maxPathPar = fst . go
  where go :: Zipper Int -> (Int, Int)
        go []             = (0, 0)
        go [Left  (n, t)] = (n + maxPath t, n)
        go [Right (n, t)] = (n + maxPath t, n)
        go zs = (max m1 (w1 + m2), w1 + w2)
          where j         = length zs `div` 2
                zl        = take j zs
                zr        = drop j zs
                (m1, w1)  = go zl
                (m2, w2)  = go zr
