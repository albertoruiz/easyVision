{-# LANGUAGE ForeignFunctionInterface,
             MagicHash,
             UnboxedTuples,
             BangPatterns,
             RecordWildCards,
             CPP #-}

-----------------------------------------------------------------------------
{- |
Module      :  Image.Core
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPLu

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Experimental interface to Intel Integrated Performance Primitives for image processing.

-}
-----------------------------------------------------------------------------


module Image.Core
          ( -- * Image representation
            Img(..), ImageType(..)
            -- * Creation of images
          , img, imgAs, getData32f, setData32f, setData8u, setValue
            -- * Regions of interest
          , fullroi, invalidROIs, validArea, roiPtrs, setRegion
            -- * Wrapper tools
          , src, checkFFI, (//), starting
            -- * Image types
          , Image(..)
          , ImageRGB(C)
          , ImageGray(G)
          , ImageFloat(F)
          , ImageDouble(D)
          , ImageYUV (Y)
          , ImageYCbCr(Y422)
          -- * Image coordinates
          , val8u, fval,
          -- * Reexported modules
          CInt, CUChar, fi, ti
          , module Image.Base, module Image.ROI,
          RawImage,
          Wrap11, wrap11,
          CInt(..), Ptr, Word8, unsafePerformIO,
          getDataFileName
) where

import Image.Base
import Image.ROI
--import ImagProc.Ipp.Structs

-- #if __GLASGOW_HASKELL__ >= 740
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr(ForeignPtr,touchForeignPtr)
-- #else
-- import Foreign.ForeignPtr
-- #endif

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad(when)
--import ImagProc.Ipp.Wrappers
import Foreign.C.String(peekCString)
import Foreign.C.Types
import GHC.Base
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Data.Binary
import Control.Applicative
import Util.Misc(assert,(//))
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString(ByteString)
import Control.DeepSeq

import Paths_hVision_base




---------------------------------------
fi :: Int -> CInt
fi = fromIntegral
ti :: CInt -> Int
ti = fromIntegral
---------------------------------------

------------------------------------------------------------
------------- descriptor of an ipp image -------------------

-- | Image type descriptor:
data ImageType = RGB | Gray | I32f | I64f | YUV | YCbCr deriving (Show,Eq)

-- | Image representation:
data Img = Img { fptr :: {-# UNPACK #-}!(ForeignPtr Word8)  -- ^ automatic allocated memory
               , ptr  :: {-# UNPACK #-}!(Ptr Word8)         -- ^ starting point of the image with the required alignment
               , step :: Int                             -- ^ number of bytes of a padded row
               , jump :: {-# UNPACK #-}!Int              -- ^ number of image elements of a padded row
               , itype :: ImageType                      -- ^ type of image
               , datasize :: Int                         -- ^ size in bytes of the base type
               , layers :: Int                           -- ^ number of layers
               , isize :: Size                           -- ^ rows and columns of the image
               , vroi :: ROI                             -- ^ ROI where data is assumed to be valid
               , bs :: B.ByteString                      -- ^ convenient image data representation
               }


img' t sz ly (Size r c) = do
    let w = c*sz*ly
        rest = w `mod` 32
        c' = if rest == 0 then w else w + 32 - rest
        tl = r*c'+31
    fp <- mallocPlainForeignPtrBytes tl
    let p' = unsafeForeignPtrToPtr fp
        p = alignPtr p' 32
        res = Img {
          isize = Size r c
        , layers = ly
        , datasize = sz
        , fptr = fp
        , ptr = p
        , step = c'
        , jump = c' `div` sz
        , itype = t
        , vroi = fullroi res
        , bs = B.PS fp (p `minusPtr` p') tl
        }
    return res

-- | Image creation. We use the Haskell gc instead of ippiMalloc and ippiFree.
img Gray = img' Gray 1 1
img RGB  = img' RGB  1 3
img I32f = img' I32f 4 1
img I64f = img' I64f 8 1
img YUV  = img' YUV  1 2 -- img' YUV ? ? -- hmm.. is 4:2:0
img YCbCr = img' YCbCr 1 2

roiPtrs :: Img -> ([Ptr Word8],Int)
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



checkFFI  :: String  -- ^ some identifier of the calling function
          -> [Img]   -- ^ the source images required by the function
          -> IO CInt  -- ^ the foreing function to wrap
          -> IO ()
-- ^ Required wrapper to any ipp function, checking that it has been successful and touching the foreign pointers of the source images to prevent early deallocation.
checkFFI msg ls f = do
    err <- f
    if (err==0)
      then
        mapM_ (touchForeignPtr . fptr) ls
      else
        error $ "error in foreign function " ++ msg

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
    theImg :: a -> Img
    appI :: RawImage t -> a -> t

setRegion (p1,p2) im = setROI (poly2roi (size im) (Closed[p1,p2])) im

appGen f im = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im

instance Image ImageFloat where
    image s = do
        i <- img I32f s
        return (F i)
    size (F Img {isize=s}) = s
    theROI (F im) = vroi im
    modifyROI f (F im) = F im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (F im) = im
    appI f (F im) = appGen f im
 
instance Image ImageDouble where
    image s = do
        i <- img I64f s
        return (D i)
    size (D Img {isize=s}) = s
    theROI (D im) = vroi im
    modifyROI f (D im) = D im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (D im) = im
    appI f (D im) = appGen f im

instance Image ImageGray where
    image s = do
        i <- img Gray s
        return (G i)
    size (G Img {isize=s}) = s
    theROI (G im) = vroi im
    modifyROI f (G im) = G im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (G im) = im
    appI f (G im) = appGen f im

instance Image ImageRGB where
    image s = do
        i <- img RGB s
        return (C i)
    size (C Img {isize=s}) = s
    theROI (C im) = vroi im
    modifyROI f (C im) = C im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (C im) = im
    appI f (C im) = appGen f im

instance Image ImageYUV where
    image s = do
        i <- img YUV s
        return (Y i)
    size (Y Img {isize=s}) = s
    theROI (Y im) = vroi im
    modifyROI f (Y im) = Y im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (Y im) = im
    appI f (Y im) = appGen f im
    
instance Image ImageYCbCr where
    image s = do
        i <- img YCbCr s
        return (Y422 i)
    size (Y422 Img {isize=s}) = s
    theROI (Y422 im) = vroi im
    modifyROI f (Y422 im) = Y422 im { vroi = f (vroi im) `intersection` (vroi im) }
    theImg (Y422 im) = im
    appI f (Y422 im) = appGen f im

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

-- | The yuv 4:2:2 image obtained by the webcam with format V4L2_PIX_FMT_YUYV
newtype ImageYCbCr = Y422 Img

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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

instance Binary Size
  where
    put (Size h w) = put h >> put w
    get = Size <$> get <*> get

instance Binary ROI
  where
    put (ROI r1 r2 c1 c2) = put r1 >> put r2 >> put c1 >> put c2
    get = ROI <$> get <*> get <*> get <*> get


instance Binary ImageRGB
  where
    put x@(C im) = put (size x) >> put (theROI x) >> (put (bs im))
    get = genGet mkRGB

instance Binary ImageGray
  where
    put x@(G im) = Util.Misc.assert (mod (step im) 32 == 0) "wrong step for put"
                 $ put (size x) >> put (theROI x) >> (put (bs im))
    get = genGet mkGray

instance Binary ImageFloat
  where
    put x@(F im) = put (size x) >> put (theROI x) >> (put (bs im))
    get = genGet mkFloat


genGet mk = do
        sz  <- get
        roi <- get
        dat <- get
        return $ mk sz roi dat


mkFloat :: Size -> ROI -> ByteString -> ImageFloat
mkFloat sz roi b@(B.PS fp o l) = unsafePerformIO $ do
    F im <- image sz
    let x = im { fptr = fp , ptr = unsafeForeignPtrToPtr fp `plusPtr` o, bs = b }
    return (setROI roi (F x))

mkGray :: Size -> ROI -> ByteString -> ImageGray
mkGray sz roi b@(B.PS fp o l) = unsafePerformIO $ do
    G im <- image sz
    let x = im { fptr = fp , ptr = unsafeForeignPtrToPtr fp `plusPtr` o, bs = b }
    return (setROI roi (G x))

mkRGB :: Size -> ROI -> ByteString -> ImageRGB
mkRGB sz roi b@(B.PS fp o l) = unsafePerformIO $ do
    C im <- image sz
    let x = im { fptr = fp , ptr = unsafeForeignPtrToPtr fp `plusPtr` o, bs = b }
    return (setROI roi (C x))

--------------------------------------------------------------------------------

instance NFData ImageRGB
  where
    rnf (C im) = rnf (bs im)

instance NFData ImageGray
  where
    rnf (G im) = rnf (bs im)

instance NFData ImageFloat
  where
    rnf (F im) = rnf (bs im)

--------------------------------------------------------------------------------

type RawImage t = Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt -> t


type Wrap11 = RawImage (RawImage (IO CInt))

wrap11 :: (Image a, Image b) => Wrap11 -> a -> b
wrap11 f x = unsafePerformIO $ do
    r <- image (size x)
    appI f x `appI` r // checkFFI "exampleInvert" [theImg x, theImg r]
    return r

