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

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dgesvd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=svdR m@ so that @m=u \<\> s \<\> 'trans' v@.
svdR :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
svdR m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_R (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svdR' m@(M r c _) = (unfortran u, s', trans_unfortran vt)
    where (u,s',vt) = svd_l_R (fortran m)

svd_l_R x@(M r c p) = createMVM [p] "svd_l_R" r r (min r c) c c $ m c_svd_l_R x
foreign import ccall "lapack-aux.h svd_l_R" c_svd_l_R :: TMMVM

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dgesdd/, which computes the full svd decomposition of a real matrix.
--
-- @(u,s,v)=full_svd_Rd m@ so that @m=u \<\> s \<\> 'trans' v@.
--full_svd_Rd :: Matrix Double -> (Matrix Double, Matrix Double , Matrix Double)
full_svd_Rd m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_Rdd (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svd_l_Rdd x@(M r c p) = createMVM [p] "svd_l_Rdd" r r (min r c) c c $ m c_svd_l_Rdd x
foreign import ccall "lapack-aux.h svd_l_Rdd" c_svd_l_Rdd :: TMMVM

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /zgesvd/, which computes the full svd decomposition of a complex matrix.
--
-- @(u,s,v)=svdC m@ so that @m=u \<\> s \<\> 'trans' v@.
svdC :: Matrix (Complex Double)
           -> (Matrix (Complex Double), Matrix Double, Matrix (Complex Double))
svdC m@(M r c _) = (unfortran u, s, trans_unfortran vt)
    where (u,s',vt) = svd_l_C (fortran m)
          s | r == c    = diag s'
            | r < c     = diag s' <|> zeros (r,c-r)
            | otherwise = diag s' <-> zeros (r-c,c)
          zeros (r,c) = reshape c $ constant 0 (r*c)

svdC' m@(M r c _) = (unfortran u, s', trans_unfortran vt)
    where (u,s',vt) = svd_l_C (fortran m)

svd_l_C x@(M r c p) = createMVM [p] "svd_l_C" r r (min r c) c c $ m c_svd_l_C x
foreign import ccall "lapack-aux.h svd_l_C" c_svd_l_C :: TCMCMVCM

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /zgeev/, which computes the eigenvalues and right eigenvectors of a general complex matrix:
--
-- if @(l,v)=eigC m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigC :: Matrix (Complex Double)
        -> (Vector (Complex Double), Matrix (Complex Double))
eigC m@(M r c _) = (s, unfortran v)
    where (_,s,v) = eig_l_C (fortran m)

eig_l_C x@(M r c p) = createMVM [p] "eig_l_C" 1 1 r r r $ m c_eig_l_C x
foreign import ccall "lapack-aux.h eig_l_C" c_eig_l_C :: TCMCMCVCM

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dgeev/, which computes the eigenvalues and right eigenvectors of a general real matrix:
--
-- if @(l,v)=eigR m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are not sorted.
eigR :: Matrix Double -> (Vector (Complex Double), Matrix (Complex Double))
eigR m@(M r c _) = (s', v'')
    where (_,s,v) = eig_l_R (fortran m)
          s' = toComplex (subVector 0 r (asReal s), subVector r r (asReal s))
          v' = toRows $ trans_unfortran v
          v'' = fromColumns $ fixeig (toList s') v'

eig_l_R x@(M r c p) = createMVM [p] "eig_l_R" 1 1 r r r $ m c_eig_l_R x
foreign import ccall "lapack-aux.h eig_l_R" c_eig_l_R :: TMMCVM

fixeig  []  _ =  []
fixeig [r] [v] = [complex v]
fixeig ((r1:+i1):(r2:+i2):r) (v1:v2:vs)
    | r1 == r2 && i1 == (-i2) = toComplex (v1,v2) : toComplex (v1,scale (-1) v2) : fixeig r vs
    | otherwise = complex v1 : fixeig ((r2:+i2):r) (v2:vs)

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /dsyev/, which computes the eigenvalues and right eigenvectors of a symmetric real matrix:
--
-- if @(l,v)=eigSl m@ then @m \<\> v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order.
eigS :: Matrix Double -> (Vector Double, Matrix Double)
eigS m@(M r c _) = (s', fliprl $ unfortran v)
    where (s,v) = eig_l_S ({-fortran-} m)
          s' = fromList . reverse . toList $  s

eig_l_S x@(M r c p) = createVM [p] "eig_l_S" r r r $ m c_eig_l_S x
foreign import ccall "lapack-aux.h eig_l_S" c_eig_l_S :: TMVM

-----------------------------------------------------------------------------

-- | Wrapper for LAPACK's /zheev/, which computes the eigenvalues and right eigenvectors of a hermitian complex matrix:
--
-- if @(l,v)=eigH m@ then @m \<\> s v = v \<\> diag l@.
--
-- The eigenvectors are the columns of v.
-- The eigenvalues are sorted in descending order.
eigH :: Matrix (Complex Double) -> (Vector Double, Matrix (Complex Double))
eigH m@(M r c _) = (s', fliprl $ unfortran v)
    where (s,v) = eig_l_H (fortran m)
          s' = fromList . reverse . toList $  s

eig_l_H x@(M r c p) = createVM [p] "eig_l_H" r r r $ m c_eig_l_H x
foreign import ccall "lapack-aux.h eig_l_H" c_eig_l_H :: TCMVCM
