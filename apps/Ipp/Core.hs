{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Experimental interface to Intel Integrated Performance Primitives for image processing.

-}
-----------------------------------------------------------------------------


module Ipp.Core
          ( -- * Image representation
            Img(..), ImageType(..), ROI(..)
            -- * Creation of images
          , img, imgAs, getData32f, setData32f, val32f
            -- * Regions of interest
          , fullroi, shrink, shift, intersection
            -- * Wrapper tools
          , src, dst, checkIPP, warningIPP, (//)
          , ippRect
) where

import Foreign hiding (shift)
import Control.Monad(when)
import Ipp.Wrappers
import Foreign.C.String(peekCString)

------------------------------------------------------------
------------- descriptor of an ipp image -------------------

-- | Image type descriptor:
data ImageType = RGB | Gray | I32f

-- | Image representation:
data Img = Img { fptr :: ForeignPtr ()  -- ^ automatic allocated memory
               , ptr  :: Ptr ()         -- ^ starting point of the image with the required alignment
               , step :: Int            -- ^ number of bytes of a padded row
               , itype :: ImageType     -- ^ type of image
               , datasize :: Int        -- ^ size in bytes of the base type
               , layers :: Int          -- ^ number of layers
               , height :: Int          -- ^ number of rows 
               , width :: Int           -- ^ number of columns
               , vroi :: ROI            -- ^ ROI where data is assumed to be valid
               }


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

-- | Image creation. We use the Haskell gc instead of ippiMalloc and ippiFree.
img :: ImageType         -- ^ type of image
       -> Int            -- ^ number of rows (height)
       -> Int            -- ^ number of columns (width)
       -> IO Img         -- ^ new image
img Gray = img' Gray 1 1
img I32f = img' I32f 4 1
img RGB  = img' RGB  1 3

-- | Extracts the data in a I32f image into a list of lists.
getData32f :: Img -> IO [[Float]]
getData32f Img {fptr = fp, ptr = p, datasize = d, step = s, height = r, width = c} = do
    let jump = s `quot` d
    let row k = peekArray c (advancePtr (castPtr p) (k*jump))
    r <- mapM row [0 .. r-1]
    touchForeignPtr fp
    return r

-- | Copies the values of list of lists the data into a I32f image. NO range checking.
setData32f :: Img -> [[Float]] -> IO ()
setData32f Img {fptr = fp, ptr = p,
               datasize = d, step = s, height = r} vs = do
    let jump = s `quot` d
    touchForeignPtr fp
    let row k l = pokeArray (advancePtr (castPtr p) (k*jump)) l
    sequence_ $ zipWith row [0..r-1] vs

-- | Returns the pixel value of an image at a given row-column. NO range checking.
val32f :: Img -> Int -> Int -> IO Float
val32f Img {fptr = fp, ptr = p, datasize = d, step = s} r c = do
    let jump = s `quot` d
    v <- peek (advancePtr (castPtr p) (r*jump+c))
    touchForeignPtr fp
    return v

data ROI = ROI { r1 :: Int  -- ^ upper row
               , r2 :: Int  -- ^ lower row
               , c1 :: Int  -- ^ leftmost column
               , c2 :: Int  -- ^ rightmost column
               } deriving Show

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

type IppRect = Double

-- | Creates an auxiliary @ippRect@ representation from  width and height.
ippRect :: Int -> Int -> IppRect
ippRect = encodeAsDouble

-- | Creates a roi with the whole size of an image.
fullroi :: Img -> ROI
fullroi img = ROI {r1=0, r2=height img-1, c1=0, c2=width img-1}

-- | Creates a new roi by reducing in (r,c) units the rows and columns or a given roi. If r or c are negative the roi expands.
shrink :: (Int,Int)  -> ROI -> ROI
shrink (r,c) roi =
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) -r,
         c1=(c1 roi) +c,
         c2=(c2 roi) -c}

-- | Creates a new roi by moving (r,c) units the position of a given roi.
shift :: (Int,Int)  -> ROI -> ROI
shift (r,c) roi =
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) +r,
         c1=(c1 roi) +c,
         c2=(c2 roi) +c}

-- | Creates a new roi as the intersection of two given roi's.
intersection :: ROI -> ROI -> ROI
intersection a b = ROI { r1 = max (r1 a) (r1 b)
                       , r2 = min (r2 a) (r2 b)
                       , c1 = max (c1 a) (c1 b)
                       , c2 = min (c2 a) (c2 b)
                       }

-- id, const

-- | Creates an image of the same type and size than a given image. Data is not copied.
imgAs :: Img -> IO Img
imgAs im = img (itype im) (height im) (width im)

-- | Extracts from a source Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
src :: Img -> ROI -> (Ptr () -> Int -> t) -> t
src im roi f = f (starting im roi) (step im)

-- | Extracts from a destination Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
dst :: Img -> ROI -> (Ptr () -> Int -> IppRect -> t) -> t
dst im roi f = f (starting im roi) (step im) (roiSize roi)

genCheckIPP act msg ls f = do
    err <- f
    when (err/=0) $ do
        putStrLn $ "WARNING: In " ++ msg ++ ":"
        ps <- ippGetStatusString err
        s <- peekCString ps
        act s
    mapM_ (touchForeignPtr . fptr) ls -- really needed!
    return ()

-- | Required wrapper to any ipp function, checking that it has been successful and touching the foreign pointers of the source images to prevent early deallocation. It the function returns an error its description is written to the console and the program aborts.
checkIPP :: String                          -- ^ some identifier of the calling function
            -> [Img]                        -- ^ the source images required by the function
            -> IO Int                       -- ^ the ipp function to wrap
            -> IO () 
checkIPP   = genCheckIPP error

-- | An alternative to 'checkIPP' which only emits a warning, without aborting the program.
warningIPP :: String -> [Img] -> IO Int -> IO ()
warningIPP = genCheckIPP putStrLn

-- | Postfix function application (@flip ($)@) for conveniently writing ipp wrappers using 'src', 'dst', and 'checkIPP'. See examples in the source code of module "Ipp.Typical".
(//) :: x -> (x -> y) -> y
infixl 0 //
(//) = flip ($)
