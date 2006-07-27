{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Basic infrastructure to access the IPP.

-}
-----------------------------------------------------------------------------


module Ipp.Core
          ( Img(..), ImageType(..)
          , img, imgAs, getData
          , ROI(..), fullroi, shrink, shift, intersection
          , src, dst, checkIPP, (//)
          , ippRect
) where

import Foreign hiding (shift)
import Control.Monad(when)
import Ipp.Wrappers
 
------------------------------------------------------------
------------- descriptor of an ipp image -------------------

data ImageType = RGB | Gray | I32f 

data Img = Img { fptr :: ForeignPtr ()
               , ptr  :: Ptr ()
               , step :: Int
               , itype :: ImageType
               , datasize :: Int
               , layers :: Int
               , height :: Int
               , width :: Int
               , vroi :: ROI
               }

-- this is the constructor, given pixel size, layers, height and width
-- we use the Haskell gc instead of ippiMalloc and ippiFree

img' t sz ly r c = do
    let w = c*sz*ly
    let rest = w `mod` 32
    let c' = if rest == 0 then w else w + 32 - rest
    fp <- mallocForeignPtrBytes (r*c'+31)
    let p' = unsafeForeignPtrToPtr fp
    let p = alignPtr p' 32
    --print (p', p) -- debug
    let res = Img { 
          height = r
        , width = c
        , layers = ly
        , datasize = sz
        , fptr = fp
        , ptr = p
        , step = c'
        , itype = t
        , vroi = fullroi res 
        }
    return res
                
img Gray = img' Gray 1 1
img I32f = img' I32f 4 1
img RGB  = img' RGB  1 3                
                
getData :: Img -> IO [[Float]]
getData (Img {fptr = fp, ptr = p, datasize = d, step = s, height = r, width = c}) = do
    let jump = s `quot` d
    let row k = peekArray c (advancePtr (castPtr p) (k*jump))
    r <- mapM row [0 .. r-1]
    touchForeignPtr fp
    return r


data ROI = ROI { r1, r2, c1, c2 :: Int} deriving Show

starting :: Img -> ROI -> Ptr ()
starting img roi = plusPtr (ptr img) (r1 roi * step img + c1 roi*(datasize img)*(layers img))
  
roiSize (ROI { r1=a, r2=b, c1=x, c2=y}) = encodeAsDouble  (y-x+1)  (b-a+1)

encodeAsDouble :: Int -> Int -> Double
encodeAsDouble a b = unsafePerformIO $ do
    p <- mallocArray 2
    pokeArray p [a,b]
    r <- peek (castPtr p)
    free p
    return r

ippRect = encodeAsDouble

fullroi img = ROI {r1=0, r2=height img-1, c1=0, c2=width img-1}

shrink (r,c) roi = 
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) -r,
         c1=(c1 roi) +c,
         c2=(c2 roi) -c}
    
shift (r,c) roi = 
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) +r,
         c1=(c1 roi) +c,
         c2=(c2 roi) +c}
       
intersection a b = ROI { r1 = max (r1 a) (r1 b)
                       , r2 = min (r2 a) (r2 b)
                       , c1 = max (c1 a) (c1 b)
                       , c2 = min (c2 a) (c2 b)
                       }
       
       
-- id, const    
    
imgAs im = img (itype im) (height im) (width im)

src im roi f = f (starting im roi) (step im)
dst im roi f = f (starting im roi) (step im) (roiSize roi)

checkIPP msg ls f = do
    err <- f
    when (err/=0) $ do
        putStrLn $ "In " ++ msg ++ ":"
        ippError err
    mapM_ (touchForeignPtr . fptr) ls -- really needed!
    return ()

infixl 0 //
(//) = flip ($)

