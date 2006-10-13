{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra.Storable
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms implemented on StorableArray.

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra.Storable (
    module Data.Array.Storable,
    Cx(..),
    IOVector, IOMatrix, IOCVector, IOCMatrix,
    mulR,
    mulC,
    eigS,
    eigH
) where

import GSL.GSL
import Data.Array.Storable
import Data.Array.MArray

data Cx = Re | Im deriving (Eq,Ord,Ix,Show)

type IOMatrix = StorableArray (Int,Int) Double
type IOVector = StorableArray Int Double
type IOCMatrix = StorableArray (Int,Int,Cx) Double
type IOCVector = StorableArray (Int,Cx) Double

rows m = do
    ((a,_),(b,_)) <- getBounds m
    return $ b-a+1
cols m = do
    ((_,a),(_,b)) <- getBounds m
    return $ b-a+1
sizes m = do
    ((r1,c1),(r2,c2)) <- getBounds m
    return (r2-r1+1,c2-c1+1)

size v = do
    (a,b) <- getBounds v
    return $ b-a+1

rows' m = do
    ((a,_,_),(b,_,_)) <- getBounds m
    return $ b-a+1
cols' m = do
    ((_,a,_),(_,b,_)) <- getBounds m
    return $ b-a+1
sizes' m = do
    ((r1,c1,_),(r2,c2,_)) <- getBounds m
    return (r2-r1+1,c2-c1+1)

size' v = do
    ((a,_),(b,_)) <- getBounds v
    return $ b-a+1

------------------------------------------------------------------

newV :: Int -> IO IOVector
newV n = newArray_ (0,n-1)

newM :: Int -> Int -> IO IOMatrix
newM r c = newArray_ ((0,0),(r-1,c-1))

newCV :: Int -> IO IOCVector
newCV n = newArray_ ((0,Re),(n-1,Im))

newCM :: Int -> Int -> IO IOCMatrix
newCM r c = newArray_ ((0,0,Re),(r-1,c-1,Im))

------------------------------------------------------------------

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

prot msg f = do
    errorcode <- f
    case errorcode of
        0    -> return ()
        1000 -> error $ "size problem in the GSL wrapper: " ++ msg
        1001 -> error $ "unknown opcode in the GSL wrapper: " ++ msg
        1002 -> error $ "memory allocation problem in GSL wrapper: " ++ msg
        1003 -> error $ "wrong file name in GSL wrapper: " ++ msg

