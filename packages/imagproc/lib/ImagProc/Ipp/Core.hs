{-# LANGUAGE ForeignFunctionInterface,
             MagicHash,
             UnboxedTuples,
             BangPatterns,
             CPP #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Experimental interface to Intel Integrated Performance Primitives for image processing.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Core
          ( -- * Image representation
            Img(..), ImageType(..)
            -- * Creation of images
          , img, imgAs, getData32f, setData32f, setData8u, setValue
            -- * Regions of interest
          , fullroi, invalidROIs, roiSZ, validArea, roiPtrs
            -- * Wrapper tools
          , src, dst, checkIPP, warningIPP, (//), starting
            -- * Image types
          , Image(..)
          , ImageRGB(C)
          , ImageGray(G)
          , ImageFloat(F)
          , ImageDouble(D)
          , ImageYUV (Y)
          -- * Image coordinates
          , val8u, fval
          -- * Reexported modules
          , module ImagProc.Ipp.Structs, CInt, CUChar, fi, ti
          , module ImagProc.Base, module ImagProc.ROI
) where

import ImagProc.Base
import ImagProc.ROI
import ImagProc.Ipp.Structs

#if __GLASGOW_HASKELL__ >= 7
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr(ForeignPtr,touchForeignPtr)
#else
import Foreign.ForeignPtr
#endif

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad(when)
import ImagProc.Ipp.Wrappers
import Foreign.C.String(peekCString)
import Foreign.C.Types
import GHC.Base
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)


---------------------------------------
fi :: Int -> CInt
fi = fromIntegral
ti :: CInt -> Int
ti = fromIntegral
---------------------------------------

------------------------------------------------------------
------------- descriptor of an ipp image -------------------

-- | Image type descriptor:
data ImageType = RGB | Gray | I32f | I64f | YUV deriving (Show,Eq)

-- | Image representation:
data Img = Img { fptr :: {-# UNPACK #-}!(ForeignPtr ())  -- ^ automatic allocated memory
               , ptr  :: {-# UNPACK #-}!(Ptr ())         -- ^ starting point of the image with the required alignment
               , step :: Int                             -- ^ number of bytes of a padded row
               , jump :: {-# UNPACK #-}!Int              -- ^ number of image elements of a padded row
               , itype :: ImageType                      -- ^ type of image
               , datasize :: Int                         -- ^ size in bytes of the base type
               , layers :: Int                           -- ^ number of layers
               , isize :: Size                           -- ^ rows and columns of the image
               , vroi :: ROI                             -- ^ ROI where data is assumed to be valid
               }


img' t sz ly (Size r c) = do
    let w = c*sz*ly
    let rest = w `mod` 32
    let c' = if rest == 0 then w else w + 32 - rest
    fp <- mallocPlainForeignPtrBytes (r*c'+31)
    let p' = unsafeForeignPtrToPtr fp
    let p = alignPtr p' 32
    --print (p', p) -- debug
    let res = Img {
          isize = Size r c
        , layers = ly
        , datasize = sz
        , fptr = fp
        , ptr = p
        , step = c'
        , jump = c' `div` sz
        , itype = t
        , vroi = fullroi res
        }
    return res

-- | Image creation. We use the Haskell gc instead of ippiMalloc and ippiFree.
img Gray = img' Gray 1 1
img RGB  = img' RGB  1 3
img I32f = img' I32f 4 1
img I64f = img' I64f 8 1
img YUV  = img' YUV  1 2 -- img' YUV ? ? -- hmm.. is 4:2:0

roiPtrs :: Img -> ([Ptr ()],Int)
roiPtrs img@Img {ptr = p, step = s, vroi = roi@(ROI r1 r2 c1 c2) } = (map row [0..r2-r1], c2-c1+1)
  where
    p' = starting img roi
    row k = plusPtr p' (k*s)



-- | to be deprecated
getData32f :: ImageFloat -> IO [[Float]]
getData32f (F Img {fptr = fp, ptr = p, datasize = d, step = s, isize = Size r c }) = do
    let jump = s `quot` d
    let row k = peekArray c (advancePtr (castPtr p) (k*jump))
    r <- mapM row [0 .. r-1]
    touchForeignPtr fp
    return r

-- | Copies the values of list of lists the data into a I32f image. NO range checking.
setData32f :: ImageFloat -> [[Float]] -> IO ()
setData32f (F Img {fptr = fp, ptr = p,
               datasize = d, step = s, isize = Size {height = r}}) vs = do
    let jump = s `quot` d
    let row k l = pokeArray (advancePtr (castPtr p) (k*jump)) l
    sequence_ $ zipWith row [0..r-1] vs
    touchForeignPtr fp --hmm

-- | Copies the values of list of lists the data into a I32f image. NO range checking.
setData8u :: ImageGray -> [[CUChar]] -> IO ()
setData8u (G Img {fptr = fp, ptr = p,
                  datasize = d, step = s, isize = Size {height = r}}) vs = do
    let jump = s `quot` d
    let row k l = pokeArray (advancePtr (castPtr p) (k*jump)) l
    sequence_ $ zipWith row [0..r-1] vs
    touchForeignPtr fp --hmm


---------------------------------------------------------------

starting :: Img -> ROI -> Ptr a
starting img roi = plusPtr (ptr img) (r1 roi * step img + c1 roi*(datasize img)*(layers img))

-- | Creates a roi with the whole size of an image.
fullroi :: Img -> ROI
fullroi Img {isize = Size h w} = ROI {r1=0, r2=h-1, c1=0, c2=w-1}


-- id, const

-- | 'ROI'\'s area in pixels
validArea :: Image a => a -> Int
validArea = roiArea . theROI


-- | Creates an image of the same type and size than a given image. Data is not copied.
imgAs :: Img -> IO Img
imgAs Img {itype=t, isize=s, datasize=d, layers=l} = img t s

-- | Extracts from a source Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
src :: Img -> ROI -> (Ptr a -> Int -> t) -> t
src im roi f = f (starting im roi) (step im)

-- | Extracts from a destination Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
dst :: Img -> ROI -> (Ptr a -> Int -> IppiSize -> t) -> t
dst im roi f = f (starting im roi) (step im) (roiSZ roi)

roiSZ = adapt . roiSize
    where adapt (Size h w) = IppiSize (fromIntegral h) (fromIntegral w)

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

--------------------------------------------------------------

-- | Given an image, invalidROIs im computes a list of ROIs surrounding the valid ROI of im.
invalidROIs :: Image a => a -> [ROI]
invalidROIs img = [r | Just r <- thefour] where
    thefour = [ if r1>0   then Just $ ROI 0 (r1-1) 0 (w-1)     else Nothing
              , if r2<h-1 then Just $ ROI (r2+1) (h-1) 0 (w-1) else Nothing
              , if c1>0   then Just $ ROI r1 r2 0 (c1-1)       else Nothing
              , if c2<w-1 then Just $ ROI r1 r2 (c2+1) (w-1)   else Nothing
              ]
    ROI r1 r2 c1 c2 = theROI img
    Size h w = size img



-- | Operations supported by the different image types.
class Image a where
    -- | creates an image of the given size
    image :: Size -> IO a
    -- | returns the size of an image
    size :: a -> Size
    -- | gets the 'ROI' of the image
    theROI :: a -> ROI
    -- | modifies the valid 'ROI' of an image in a \"safe\" way
    -- (the new ROI is the intersection of the desired ROI and the old valid ROI)
    modifyROI :: (ROI->ROI) -> a -> a
    -- | modifyROI . const
    setROI :: ROI -> a -> a
    setROI = modifyROI . const

instance Image ImageFloat where
    image s = do
        i <- img I32f s
        return (F i)
    size (F Img {isize=s}) = s
    theROI (F im) = vroi im
    modifyROI f (F im) = F im { vroi = f (vroi im) `intersection` (vroi im) }

instance Image ImageDouble where
    image s = do
        i <- img I64f s
        return (D i)
    size (D Img {isize=s}) = s
    theROI (D im) = vroi im
    modifyROI f (D im) = D im { vroi = f (vroi im) `intersection` (vroi im) }

instance Image ImageGray where
    image s = do
        i <- img Gray s
        return (G i)
    size (G Img {isize=s}) = s
    theROI (G im) = vroi im
    modifyROI f (G im) = G im { vroi = f (vroi im) `intersection` (vroi im) }

instance Image ImageRGB where
    image s = do
        i <- img RGB s
        return (C i)
    size (C Img {isize=s}) = s
    theROI (C im) = vroi im
    modifyROI f (C im) = C im { vroi = f (vroi im) `intersection` (vroi im) }

instance Image ImageYUV where
    image s = do
        i <- img YUV s
        return (Y i)
    size (Y Img {isize=s}) = s
    theROI (Y im) = vroi im
    modifyROI f (Y im) = Y im { vroi = f (vroi im) `intersection` (vroi im) }


-- | The IPP 8u_C3 image type
newtype ImageRGB   = C Img

-- | The IPP 8u_C1 image type
newtype ImageGray  = G Img

-- | The IPP 32f_C1 image type
newtype ImageFloat = F Img

-- | The IPP 64f_C1 image type (used mainly as an auxiliary buffer for certain operations)
newtype ImageDouble = D Img

-- | The yuv 4:2:0 image obtained by mplayer -vo yuv4mpeg
newtype ImageYUV = Y Img


-------------------------------------------------------------------

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}


-- | Returns the pixel value of an image at a given pixel. NO range checking.
fval :: ImageFloat -> Pixel -> Float
fval (F Img {fptr = fp, ptr = p, jump = j}) (Pixel r c) = inlinePerformIO $ do
    v <- peek (advancePtr (castPtr p) (r*j+c))
    touchForeignPtr fp
    return v
{-# INLINE fval #-}


-- | Returns the pixel value of an image at a given pixel with range checking
val32f' :: ImageFloat -> Pixel -> IO Float
val32f' (F Img {fptr = fp, ptr = p, datasize = d, step = s, vroi = ROI r1 r2 c1 c2}) (Pixel r c) = do
    when (r<r1 || c<c1 || r > r2 || c > c2) $ error "val32f out of range"
    let jump = s `quot` d
    v <- peek (advancePtr (castPtr p) (r*jump+c))
    touchForeignPtr fp
    return v

-- | Returns the value of an image at a given pixel. NO range checking.
val8u :: ImageGray -> Pixel -> CUChar
val8u (G Img {fptr = fp, ptr = p, jump=j}) (Pixel r c) = inlinePerformIO $ do
    v <- peek (advancePtr (castPtr p) (r*j+c))
    touchForeignPtr fp
    return v
{-# INLINE val8u #-}

------------------------------------------------------------------

-- | Sets the pixel value of an image at a given row-column. NO range checking.
setValue :: (Storable b) => Img -> b -> Int -> Int -> IO ()
setValue Img {fptr = fp, ptr = p, jump = j} v r c = do
    poke (advancePtr (castPtr p) (r*j+c)) v
    touchForeignPtr fp

-------------------------------------------------------------------

