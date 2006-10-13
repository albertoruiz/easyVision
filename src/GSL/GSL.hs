{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.GSL
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Interface to the GSL functions (<http://www.gnu.org/software/gsl>).
--
-----------------------------------------------------------------------------

module GSL.GSL where

import Foreign
import Foreign.C.Types

------------------------------------------------
---------- signatures of the C functions -------
------------------------------------------------
type PD = Ptr Double                          --
type PC = Ptr Double --(Complex Double)       --
type TV = Int -> PD -> IO Int                 --
type TVV = Int -> PD -> TV                    --
type TVVV = Int -> PD -> TVV                  -- 
type TM = Int -> Int -> PD -> IO Int          -- 
type TMM =  Int -> Int -> PD -> TM            -- 
type TMMM =  Int -> Int -> PD -> TMM          -- 
type TVM = Int -> PD -> TM                    -- 
type TVVM = Int -> PD -> TVM                  --
type TMV = Int -> Int -> PD -> TV             -- 
type TMVM = Int -> Int -> PD -> TVM           -- 
type TMMVM = Int -> Int -> PD -> TMVM         --
type TCM = Int -> Int -> PC -> IO Int         --
type TCVCM = Int -> PC -> TCM                 -- 
type TCMCM = Int -> Int -> PC -> TCM          -- 
type TVCM = Int -> PD -> TCM                  --
type TCMVCM = Int -> Int -> PC -> TVCM        --
type TCMCMCM = Int -> Int -> PC -> TCMCM      -- 
type TCV = Int -> PC -> IO Int                -- 
type TCVCV = Int -> PC -> TCV                 --
type TCMCV = Int -> Int -> PC -> TCV          --
type TVCV = Int -> PD -> TCV                  -- 
------------------------------------------------

foreign import ccall "gslaux.h constant" c_constant :: Double -> TV

foreign import ccall "gslaux.h diagR" c_diagR :: TVM

foreign import ccall "gslaux.h diagC" c_diagC :: TCVCM

foreign import ccall "gslaux.h take_diagonal" c_take_diagonal :: TMV

foreign import ccall "gslaux.h take_diagonalC" c_take_diagonalC :: TCMCV

foreign import ccall "gslaux.h eigensystemR" c_eigS :: TMVM

foreign import ccall "gslaux.h eigensystemC" c_eigH :: TCMVCM

foreign import ccall "gslaux.h svd" c_svd :: TMMVM

foreign import ccall "gslaux.h QR" c_qr :: TMMM

foreign import ccall "gslaux.h chol" c_chol :: TMM

foreign import ccall "gslaux.h multiplyR" c_mulR :: TMMM

foreign import ccall "gslaux.h multiplyC" c_mulC :: TCMCMCM

foreign import ccall "gslaux.h trans" c_trans :: TMM

foreign import ccall "gslaux.h transC" c_transC :: TCMCM

foreign import ccall "gslaux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

foreign import ccall "gslaux.h vector_scale" c_vectorScale :: Double -> TVV

foreign import ccall "gslaux.h vector_offset" c_vectorOffset :: Double -> TVV

foreign import ccall "gslaux.h toScalar" c_toScalar :: Int -> TVV

foreign import ccall "gslaux.h vectorMap" c_vectorMap :: Int -> TVV

foreign import ccall "gslaux.h vectorZip" c_vectorZip :: Int -> TVVV

foreign import ccall "gslaux.h luSolveR" c_luSolveR ::  TMMM

foreign import ccall "gslaux.h luSolveC" c_luSolveC ::  TCMCMCM

foreign import ccall "gslaux.h luRaux" c_luRaux :: TMV

foreign import ccall "gslaux.h luCaux" c_luCaux :: TCMCV

foreign import ccall "gslaux.h matrix_fscanf" c_gslReadMatrix:: Ptr CChar -> TM

foreign import ccall "gslaux.h mesh" c_mesh :: Int -> Int -> Ptr Double -> IO Int
