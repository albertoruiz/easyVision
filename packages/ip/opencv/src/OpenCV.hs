{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module OpenCV(
  canny,
  hough,
  cascadeClassifier,
  solvePNP,
  findHomography, findHomographyRANSAC, findHomographyLMEDS,
  estimateRigidTransform,
  MotionType(..), findTransformECC,
  surf,
  warp8u, warp8u3, warp32f, CVPix(..), warp,
  undistort8u,
  webcam
) where

import Image.Devel
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types(CUChar)
import Foreign(Word8)
import Foreign.Storable
import Util.Geometry(Segment(..),Point(..))
import Control.Applicative
import Data.Packed.Development
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((¦))
import Numeric.LinearAlgebra.HMatrix((===),row,(!))
import Util.Homogeneous(rodrigues)
import Control.Arrow((&&&))

--------------------------------------------------------------------------------

type M = Matrix Double
type V = Vector Double

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

foreign import ccall "opencv_undistort8u"
    c_opencv_undistort8u :: MAT (VEC (MAT (RawImageS I8u (RawImageS I8u (IO CInt)))))

undistort8u :: M -> V -> M -> Image I8u -> Image I8u
undistort8u k d nk x = unsafePerformIO $ do
    r <- newImage undefined (size x)
    let cm  = cmat k
        cm2 = cmat nk
    withCMatrix cm $ withVector d $ withCMatrix cm2 $ withImage x $ withImage r $ checkFFI "opencv_undistort8u" $
        c_opencv_undistort8u `appMat` cm `appVec` d `appMat` cm2 `appS` x `appS` r
    return r

------------------------------------------------------------------

wrap11SHS :: Storable b => String -> MAT (Wrap11S a b) -> Size -> M -> Image a -> Image b
wrap11SHS msg f sz m x = unsafePerformIO $ do
    r <- newImage undefined sz
    let cm = cmat $ inv (pixelToPointTrans (size r)) <> m <> pixelToPointTrans (size x)
    withCMatrix cm $ withImage x $ withImage r $ checkFFI msg $
        f `appMat` cm `appS` x `appS` r
    return r

warp8u :: Word8 -> Size -> M -> Image I8u -> Image I8u
warp8u g = wrap11SHS "opencv_warp8u" (c_opencv_warp8u 1 g)

warp8u3 :: Word8 -> Size -> M -> Image I8u3 -> Image I8u3
warp8u3 g = wrap11SHS "opencv_warp8u3" (c_opencv_warp8u3 1 g)

warp32f :: Float -> Size -> M -> Image Float -> Image Float
warp32f g = wrap11SHS "opencv_warp32f" (c_opencv_warp32f 1 g)

iowarpg :: Num q => String -> (CInt -> q -> MAT (RawImageS p (RawImageS p (IO CInt)))) -> M -> Image p -> Image p -> IO ()
iowarpg msg f m x r = do
    let cm = cmat $ inv (pixelToPointTrans (size r)) <> m <> pixelToPointTrans (size x)
    withCMatrix cm $ withImage x $ withImage r $ checkFFI msg $
        f 0 0 `appMat` cm `appS` x `appS` r

iowarp8u :: M -> Image I8u -> Image I8u -> IO ()
iowarp8u = iowarpg "iowarp8u" c_opencv_warp8u

iowarp8u3 :: M -> Image I8u3 -> Image I8u3 -> IO ()
iowarp8u3 = iowarpg "iowarp8u3" c_opencv_warp8u3

iowarp32f :: M -> Image Float -> Image Float -> IO ()
iowarp32f = iowarpg "iowarp32f" c_opencv_warp32f

warpong ::  (M -> Image p -> Image p -> IO ()) -> Image p -> [(M,Image p)] -> Image p
warpong f base hxs = unsafePerformIO $ do
    res <- cloneImage base
    mapM_ (g res) hxs
    return res
  where
    g res (h,x)
        | ok = f h x res
        | otherwise = error $ "warpon with transformation "++show shz
      where
        shz = (rows &&& cols) h
        ok = shz == (3,3)

class BPix p => CVPix p
  where
    warpon :: Image p -> [(Matrix Double,Image p)] -> Image p

instance CVPix I8u
  where
    warpon = warpong iowarp8u

instance CVPix I8u3
  where
    warpon = warpong iowarp8u3

instance CVPix Float
  where
    warpon = warpong iowarp32f

warp :: CVPix p => p -> Size -> M -> Image p -> Image p
warp v sz h x = warpon (constantImage v sz) [(h,x)]


foreign import ccall "opencv_warp8u"
    c_opencv_warp8u :: CInt -> Word8 -> MAT (RawImageS I8u (RawImageS I8u (IO CInt)))

foreign import ccall "opencv_warp8u3"
    c_opencv_warp8u3 :: CInt -> Word8 -> MAT (RawImageS I8u3 (RawImageS I8u3 (IO CInt)))

foreign import ccall "opencv_warp32f"
    c_opencv_warp32f :: CInt -> Float -> MAT (RawImageS Float (RawImageS Float (IO CInt)))

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
    return $ k <> (rodrigues rv ¦ asColumn t)

foreign import ccall unsafe "cPNP"
    c_PNP :: CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> CInt -> CInt -> Ptr Double
          -> IO CInt

--------------------------------------------------------------------------------

type Mask = Vector CUChar

estimateRigidTransform :: Matrix Double -> Matrix Double -> Matrix Double
estimateRigidTransform vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 2 3
    mask <- createVector (rows vs)
    app4 (c_FindHomography 3 0) mat (cmat vs) mat (cmat ps) mat r vec mask "estimateRigidTransform"
    return (r === row [0,0,1])

findHomography :: Matrix Double -> Matrix Double -> Matrix Double
findHomography vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 3 3
    mask <- createVector (rows vs)
    app4 (c_FindHomography 0 0) mat (cmat vs) mat (cmat ps) mat r vec mask "findHomography"
    return r

findHomographyRANSAC :: Double -> M -> M -> (M,[Int])
findHomographyRANSAC th vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 3 3
    mask <- createVector (rows vs)
    app4 (c_FindHomography 1 th) mat (cmat vs) mat (cmat ps) mat r vec mask "findHomographyRANSAC"
    return (r, findMask mask)

findHomographyLMEDS :: M -> M -> (M,[Int])
findHomographyLMEDS vs ps = unsafePerformIO $ do
    r <- createMatrix RowMajor 3 3
    mask <- createVector (rows vs)
    app4 (c_FindHomography 2 0) mat (cmat vs) mat (cmat ps) mat r vec mask "findHomographyLMEDS"
    return (r, findMask mask)

foreign import ccall unsafe "cFindHomography" c_FindHomography
    :: CInt -> Double -> MAT (MAT (MAT (CInt -> Ptr CUChar -> IO CInt)))

findMask :: Mask -> [Int]
findMask = find (>0.5) . fromList . map (fromIntegral :: CUChar -> Float) . toList

--------------------------------------------------------------------------------

data MotionType
    = Translation
    | Euclidean
    | Affine
    | Projective
  deriving (Eq, Enum, Read, Show)

findTransformECC :: MotionType -> Image I8u -> Image I8u -> M -> (M,Double)
findTransformECC mot dst src h0 = unsafePerformIO $ do
    let hr = if mot == Projective then 3 else 2
    h <- createMatrix RowMajor hr 3
    let ch0  = cmat $ takeRows hr $ h0 / scalar (h0!2!2) -- inv (pixelToPointTrans (size r)) <> h0 <> pixelToPointTrans (size x)
        code = fromIntegral (fromEnum mot)
    r <- withCMatrix ch0 $ withCMatrix h $ withImage dst $ withImage src $ do
        c_findTransformECC code `appS` src `appS` dst `appMat` ch0 `appMat` h
    let oh = if mot == Projective then h else h === row [0,0,1]
    return (oh,r)

foreign import ccall "cFindTransformECC"
    c_findTransformECC :: CInt -> RawImageS I8u (RawImageS I8u (MAT (MAT (IO Double))))

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

--------------------------------------------------------------------------------

foreign import ccall "openOCVC"
    c_openOCVC :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr ())

foreign import ccall "grabOCVC"
    c_grabOCVC :: Ptr () -> RawImageS I8u3 (IO CInt)


webcam
  :: String  -- ^ device
  -> Size    -- ^ requested size
  -> Int     -- ^ frame rate
  -> IO (IO (Maybe ImageRGB))
webcam d (Size h w) fps = do
    let dev = read [last d]
    pw <- new (fi w)
    ph <- new (fi h)
    pf <- new (fi fps)
    han <- c_openOCVC dev pw ph pf
    tw <- ti <$> peek pw
    th <- ti <$> peek ph
    mapM_ free [pw,ph,pf]
    return $ do
        im <- newImage undefined (Size th tw)
        ok <- c_grabOCVC han `appS` im
        if ok==0
          then return (Just im)
          else return Nothing

