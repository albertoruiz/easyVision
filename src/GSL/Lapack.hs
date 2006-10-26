{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Lapack
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Wrappers for a few LAPACK functions (<http://www.netlib.org/lapack>).
--
-----------------------------------------------------------------------------

-- #hide
module GSL.Lapack where

import GSL.Types
import GSL.Wrappers
import GSL.Common
import Foreign
import Foreign.C.Types
import Foreign.C.String

-- | changes from row order (C) to column order (Fortran)
fortran m = M r c p' where M c r p' = trans m
-- | changes from column order (Fortran) to row order (C)
unfortran (M r c p') = M r c p where M _ _ p = trans (M c r p')

-- | wrapper to lapack dgesdd, job \'A\', which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full_svd_R m@ so that @m=u \<\> s \<\> 'trans' v@.
full_svd_R :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
full_svd_R m@(M r c _) = (unfortran u, s, trans $ unfortran vt)
    where (u,s',vt) = svd_l (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svd_l x@(M r c p) = createMVM [p] "svd_l" r r (min r c) c c $ m c_svd_l x
foreign import ccall "lapack-aux.h svd_l" c_svd_l :: TMMVM
