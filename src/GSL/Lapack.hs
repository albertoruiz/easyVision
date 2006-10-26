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

trans_unfortran (M r c p) = M c r p

-- | wrapper to lapack dgesvd, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full_svd_R m@ so that @m=u \<\> s \<\> 'trans' v@.
full_svd_R :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
full_svd_R m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_R (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svd_l_R x@(M r c p) = createMVM [p] "svd_l_R" r r (min r c) c c $ m c_svd_l_R x
foreign import ccall "lapack-aux.h svd_l_R" c_svd_l_R :: TMMVM

-- | wrapper to lapack dgesdd, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full_svd_Rd m@ so that @m=u \<\> s \<\> 'trans' v@.
full_svd_Rd :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
full_svd_Rd m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_Rdd (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svd_l_Rdd x@(M r c p) = createMVM [p] "svd_l_Rdd" r r (min r c) c c $ m c_svd_l_Rdd x
foreign import ccall "lapack-aux.h svd_l_Rdd" c_svd_l_Rdd :: TMMVM


-- | wrapper to lapack zgesvd, which computes the full svd decomposition of a complex matrix.
--
-- @(u,s,v)=full_svd_C m@ so that @m=u \<\> s \<\> 'trans' v@.
full_svd_C :: Matrix (Complex Double)
           -> (Matrix (Complex Double), Matrix Double, Matrix (Complex Double))
full_svd_C m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_C (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svd_l_C x@(M r c p) = createMVM [p] "svd_l_C" r r (min r c) c c $ m c_svd_l_C x
foreign import ccall "lapack-aux.h svd_l_C" c_svd_l_C :: TCMCMVCM
