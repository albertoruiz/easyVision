{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCV(
  canny,
  hough,
  cascadeClassifier,
  solvePNP,
  findHomography,
  surf
) where

import Image.Devel
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.Storable
import Util.Geometry(Segment(..),Point(..))
import Control.Applicative
import Data.Packed.Development
import Numeric.LinearAlgebra
import Util.Homogeneous(rodrigues)

--------------------------------------------------------------------------------

data Rect = Rect !Pixel !Size

instance Storable Rect where
  sizeOf _ = 4*sizeOf (undefined::CInt)
  alignment _ = alignment (undefined::CInt)
  
  peek p = do
    let r k = fromIntegral <$> peekElemOff (castPtr p :: Ptr CInt) k
    Rect <$> (Pixel <$> r 0 <*> r 1) <*> (Size <$> r 2 <*> r 3)
  
  poke p (Rect (Pixel r c) (Size h w)) = do
    let s k = pokeElemOff (castPtr p:: Ptr CInt) k . fromIntegral
    s 0 r
    s 1 c
    s 2 h
    s 3 w

------------------------------------------------------------------

canny :: Image I8u -> Image I8u
canny = wrap11S "opencv_canny" c_opencv_canny

foreign import ccall "opencv_canny"
    c_opencv_canny :: RawImageS I8u (RawImageS I8u (IO CInt))

------------------------------------------------------------------

foreign import ccall "hough"
    c_hough :: RawImageS I8u (CInt -> Ptr CInt -> Ptr Segment -> IO ())

hough :: Int -> Image I8u -> [Segment]
hough fmax imag = unsafePerformIO $ do
    pn <- malloc
    pf <- mallocArray fmax
    tmp <- cloneImage imag
    withImage imag $ do
        (c_hough `appS` tmp) (fi fmax) pn pf
    n <- peek pn
    r <- peekArray (ti n) pf
    free pn
    free pf
    return r

-------------------------------------------------------------------

foreign import ccall "initCascade"
    c_initCascade :: CString -> IO (Ptr ())

foreign import ccall "cascadeDetect"
    c_cascadeDetect :: Ptr () -> RawImageS I8u (CInt -> Ptr CInt -> Ptr Rect -> IO ())

cascadeDetect :: Ptr () -> Image I8u -> Int -> [Rect]
cascadeDetect casc imag fmax = unsafePerformIO $ do
    pn <- malloc
    pf <- mallocArray fmax
    withImage imag $ do
        appS (c_cascadeDetect casc) imag (fi fmax) pn pf
    n <- peek pn
    r <- peekArray (ti n) pf
    free pn
    free pf
    return r


cascadeClassifier :: FilePath -> IO (Image I8u -> Int -> [ROI])
cascadeClassifier file = do
    cascade <- newCString file
    c <- c_initCascade cascade
    free cascade
    return (\x n -> map rect2roi (cascadeDetect c x n))
  where
    rect2roi (Rect p s) = mkROI p s

--------------------------------------------------------------------------------

solvePNP :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
solvePNP k vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 2 3
    app4 c_PNP mat (cmat k) mat (cmat vs) mat (cmat ps) mat r "c_PNP"
    let [rv,t] = toRows r
    return $ k <.> (rodrigues rv ¦ asColumn t)

foreign import ccall unsafe "cPNP"
    c_PNP :: CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> IO CInt

--------------------------------------------------------------------------------

findHomography :: Matrix Double -> Matrix Double -> Matrix Double
findHomography vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 3 3
    app3 c_FindHomography mat (cmat vs) mat (cmat ps) mat r "findHomography"
    return r

foreign import ccall unsafe "cFindHomography" c_FindHomography
    :: CInt -> CInt -> Ptr Double
    -> CInt -> CInt -> Ptr Double
    -> CInt -> CInt -> Ptr Double
    -> IO CInt

--------------------------------------------------------------------------------

foreign import ccall "surf"
    c_surf :: RawImageS I8u (CInt -> Ptr CInt -> Ptr Point -> IO ())

surf :: Int -> Image I8u -> [Point]
surf fmax imag = unsafePerformIO $ do
    pn <- malloc
    pf <- mallocArray fmax
    tmp <- cloneImage imag
    withImage imag $ do
        (c_surf `appS` tmp) (fi fmax) pn pf
    n <- peek pn
    r <- peekArray (ti n) pf
    free pn
    free pf
    return r

