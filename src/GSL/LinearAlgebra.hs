{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms, implemented by means of the GSL or Lapack.

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra (
    module GSL.Vector,
    module GSL.Matrix
    mulR,
    mulC,
    eigS,
    eigH,
) where

import GSL.Vector
import GSL.Matrix


mulR :: IOMatrix -> IOMatrix -> IO IOMatrix
mulR a b = do
    (ar,ac) <- sizes a
    (br,bc) <- sizes b 
    c <- newM ar bc
    withStorableArray a $ \pa ->
        withStorableArray b $ \pb ->
            withStorableArray c $ \pc -> do
                prot "mulR" $ c_mulR ar ac pa br bc pb ar bc pc
    return c
------------------------------------------------------------------

mulC :: IOCMatrix -> IOCMatrix -> IO (IOCMatrix)
mulC a b = do
    (ar,ac) <- sizes' a
    (br,bc) <- sizes' b
    c <- newCM ar bc
    withStorableArray a $ \pa ->
        withStorableArray b $ \pb ->
            withStorableArray c $ \pc -> do
                prot "mulC" $ c_mulC ar ac pa br bc pb ar bc pc
    return c

------------------------------------------------------------------

eigS :: IOMatrix -> IO (IOVector, IOMatrix)
eigS x = do
    (r,c) <- sizes x
    l <- newV r
    v <- newM r r
    withStorableArray x $ \px ->
        withStorableArray v $ \pv ->
            withStorableArray l $ \pl ->
                prot "eigS" $ c_eigS r c px r pl r r pv
    return (l,v)

------------------------------------------------------------------

eigH :: IOCMatrix -> IO (IOVector, IOCMatrix)
eigH x = do
    (r,c) <- sizes' x
    l <- newV r
    v <- newCM r r
    withStorableArray x $ \px ->
        withStorableArray v $ \pv ->
            withStorableArray l $ \pl ->
                prot "eigH" $ c_eigH r c px r pl r r pv
    return (l,v)

-----------------------------------------------------------------

-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
gslReadMatrix :: FilePath -> (Int,Int) -> IO (IOMatrix)
gslReadMatrix filename (r,c) = do
    charname <- newCString filename
    m <- newM r c
    withStorableArray m $ \pm ->
        prot "gslReadMatrix" $ c_gslReadMatrix charname r c pm
    --free charname  -- TO DO: free the auxiliary CString
    return m


prot msg f = do
    errorcode <- f
    case errorcode of
        0    -> return ()
        1000 -> error $ "size problem in the GSL wrapper: " ++ msg
        1001 -> error $ "unknown opcode in the GSL wrapper: " ++ msg
        1002 -> error $ "memory allocation problem in GSL wrapper: " ++ msg
        1003 -> error $ "wrong file name in GSL wrapper: " ++ msg

