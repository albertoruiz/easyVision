{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Experimental interface to Intel Integrated Performance Primitives for image processing.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Core
          ( -- * Image representation
            Img(..), ImageType(..), ROI(..), Size(..)
            -- * Creation of images
          , img, imgAs, getData32f, setData32f, value
            -- * Regions of interest
          , fullroi, shrink, shift, intersection, roiArea, invalidROIs
            -- * Wrapper tools
          , src, dst, checkIPP, warningIPP, (//), purifyWith
          , ippRect, roiSize
            -- * Image types
          , Image(..)
          , ImageRGB(C)
          , ImageGray(G)
          , ImageFloat(F)
          , ImageYUV (Y)
          -- * Image coordinates
          , Pixel (..)
          , Point (..)
          , Segment(..)
          , segmentLength, distPoints
          , pixelsToPoints, pixelToPointTrans
          , val32f, val32f', val8u
) where

import Foreign hiding (shift)
import Control.Monad(when)
import ImagProc.Ipp.Wrappers
import Foreign.C.String(peekCString)
import Foreign.C.Types
import qualified GSL
import Vision

------------------------------------------------------------
------------- descriptor of an ipp image -------------------

data Size  = Size  {height :: !Int, width :: !Int} deriving Show

-- | Image type descriptor:
data ImageType = RGB | Gray | I32f | YUV deriving (Show,Eq)

-- | Image representation:
data Img = Img { fptr :: ForeignPtr ()  -- ^ automatic allocated memory
               , ptr  :: Ptr ()         -- ^ starting point of the image with the required alignment
               , step :: Int            -- ^ number of bytes of a padded row
               , itype :: ImageType     -- ^ type of image
               , datasize :: Int        -- ^ size in bytes of the base type
               , layers :: Int          -- ^ number of layers
               , isize :: Size          -- ^ rows and columns of the image
               , vroi :: ROI            -- ^ ROI where data is assumed to be valid
               }


img' t sz ly (Size r c) = do
    let w = c*sz*ly
    let rest = w `mod` 32
    let c' = if rest == 0 then w else w + 32 - rest
    fp <- mallocForeignPtrBytes (r*c'+31)
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
        , itype = t
        , vroi = fullroi res 
        }
    return res

-- | Image creation. We use the Haskell gc instead of ippiMalloc and ippiFree.
img Gray = img' Gray 1 1
img RGB  = img' RGB  1 3
img I32f = img' I32f 4 1
img YUV  = img' YUV  1 2 -- img' YUV ? ? -- hmm.. is 4:2:0

-- | Extracts the data in a I32f image into a list of lists.
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

-- | Returns the pixel value of an image at a given row-column. NO range checking.
value :: (Storable b) => Img -> Int -> Int -> IO b
value Img {fptr = fp, ptr = p, datasize = d, step = s} r c = do
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
fullroi Img {isize = Size h w} = ROI {r1=0, r2=h-1, c1=0, c2=w-1}

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

-- | 'ROI'\'s area in pixels
roiArea :: Image a => a -> Int
roiArea im = w*h where
    ROI r1 r2 c1 c2 = theROI im
    w = c2-c1+1
    h = r2-r1+1



-- | Creates an image of the same type and size than a given image. Data is not copied.
imgAs :: Img -> IO Img
imgAs Img {itype=t, isize=s, datasize=d, layers=l} = img t s

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

-- | Given an IO function which is essentially pure except for possibly undefined data outside the ROI, it creates a pure function with the same effect, which applies a given initialization to the region outside the roi. For example, @purifyWith ("set32f" 0) (iofun args)@ creates a pure fun equal to iofun with zero pixels outside the ROI; to copy the data from an image you can use @("copyROI32f" im)@, etc.
purifyWith :: Image a => (ROI -> a -> IO ()) -> (IO a) -> a
purifyWith roifun imgfun = unsafePerformIO $ do
    r <- imgfun
    mapM_ ((flip roifun) r) (invalidROIs r)
    return r

-- | Operations supported by the different image types.
class Image a where
    -- | creates an image of the given size
    image :: Size -> IO a
    -- | returns the size of an image
    size :: a -> Size
    -- | gets the 'ROI' of the image
    theROI :: a -> ROI
    -- | modifies the 'ROI' of an image
    modifyROI :: (ROI->ROI) -> a -> a

instance Image ImageFloat where
    image s = do
        i <- img I32f s
        return (F i)
    size (F Img {isize=s}) = s
    theROI (F im) = vroi im
    modifyROI f (F im) = F im { vroi = f (vroi im) }

instance Image ImageGray where
    image s = do
        i <- img Gray s
        return (G i)
    size (G Img {isize=s}) = s
    theROI (G im) = vroi im
    modifyROI f (G im) = G im { vroi = f (vroi im) }

instance Image ImageRGB where
    image s = do
        i <- img RGB s
        return (C i)
    size (C Img {isize=s}) = s
    theROI (C im) = vroi im
    modifyROI f (C im) = C im { vroi = f (vroi im) }

instance Image ImageYUV where
    image s = do
        i <- img YUV s
        return (Y i)
    size (Y Img {isize=s}) = s
    theROI (Y im) = vroi im
    modifyROI f (Y im) = Y im { vroi = f (vroi im) }


-- | The IPP 8u_C3 image type
newtype ImageRGB   = C Img

-- | The IPP 8u_C1 image type
newtype ImageGray  = G Img

-- | The IPP 32f_C1 image type
newtype ImageFloat = F Img

-- | The yuv 4:2:0 image obtained by mplayer -vo yuv4mpeg
newtype ImageYUV = Y Img

-- | Normalized image coordinates, with x from +1 to -1 (for a right handed 3D reference system with z pointing forward)
data Point = Point { px    :: !Double, py :: !Double} deriving (Eq, Show)

-- | Raw image coordinates
data Pixel = Pixel { row   :: !Int,    col :: !Int } deriving (Eq, Show)

-- | Auxiliary homogeneous transformation from 'Pixel's to 'Point's
pixelToPointTrans :: Size -> GSL.Matrix Double
pixelToPointTrans Size {width = w', height = h'} = nor where
    w = fromIntegral w'
    h = fromIntegral h'
    r = h/w
    nor = GSL.fromLists
        [[-2/w,      0, 1]
        ,[   0, -2*r/h, r]
        ,[   0,      0, 1]]

pixelToList Pixel {row = r, col = c} = [fromIntegral c, fromIntegral r]
listToPoint [x,y] = Point {px = x, py= y}

-- | Trasformation from pixels to normalized points.
pixelsToPoints :: Size -> [Pixel]->[Point]
pixelsToPoints sz = fix where
    nor = pixelToPointTrans sz
    fix = map listToPoint. ht nor . map pixelToList


data Segment = Segment {
    extreme1 :: !Point,
    extreme2 :: !Point
}

-- | The length of a segment.
segmentLength :: Segment -> Double
segmentLength (Segment {extreme1 = e1, extreme2 = e2}) = distPoints e1 e2

-- | Euclidean distance between two points
distPoints :: Point -> Point -> Double
distPoints (Point a b) (Point x y) = sqrt $ (a-x)^2+(b-y)^2






-- | Returns the pixel value of an image at a given pixel. NO range checking.
val32f :: ImageFloat -> Pixel -> IO Float
val32f (F Img {fptr = fp, ptr = p, datasize = d, step = s}) (Pixel r c) = do
    let jump = s `quot` d
    v <- peek (advancePtr (castPtr p) (r*jump+c))
    touchForeignPtr fp
    return v

-- | Returns the pixel value of an image at a given pixel with range checking
val32f' :: ImageFloat -> Pixel -> IO Float
val32f' (F Img {fptr = fp, ptr = p, datasize = d, step = s, vroi = ROI r1 r2 c1 c2}) (Pixel r c) = do
    when (r<r1 || c<c1 || r > r2 || c > c2) $ error "val32f out of range"
    let jump = s `quot` d
    v <- peek (advancePtr (castPtr p) (r*jump+c))
    touchForeignPtr fp
    return v

-- | Returns the pixel value of an image at a given pixel. NO range checking.
val8u :: ImageGray -> Pixel -> CUChar
val8u (G Img {fptr = fp, ptr = p, datasize = d, step = s}) (Pixel r c) = unsafePerformIO $ do
    v <- peek (advancePtr (castPtr p) (r*s+c))
    touchForeignPtr fp
    return v
