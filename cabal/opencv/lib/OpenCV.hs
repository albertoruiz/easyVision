{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCV(
  testImage8u,
  hough,
  cascadeClassifier,

  module OpenCV.MSER
) where

import ImagProc.Ipp.Core
import Foreign
import Foreign.C.String

import OpenCV.MSER

------------------------------------------------------------------

app1G f (G im) =
    f (ptr im) (fi.step $ im) (fi $ width $ isize im) (fi $ height $ isize im)
      (fi $ r1 $ vroi im) (fi $ r2 $ vroi im) (fi.c1.vroi $ im) (fi.c2.vroi $ im)

------------------------------------------------------------------

testImage8u :: ImageGray -> IO ()
testImage8u = app1G c_testImage8u

foreign import ccall "testImage8u"
    c_testImage8u :: Ptr () -> CInt -> CInt -> CInt
                     -> CInt -> CInt -> CInt -> CInt -> IO ()

------------------------------------------------------------------

foreign import ccall "hough"
    c_hough :: Ptr () -> CInt -> CInt -> CInt
            -> CInt -> CInt -> CInt -> CInt
            -> CInt -> Ptr CInt -> Ptr IppiRect -> IO ()

hough :: ImageGray -> Int -> [Segment]
hough imag fmax = unsafePerformIO $ do
    pn <- malloc
    pf <- mallocArray fmax
    app1G c_hough imag (fi fmax) pn pf
    n <- peek pn
    r <- peekArray (ti n) pf
    free pn
    free pf
    return (map (rect2Segment (theROI imag) (size imag)) r)


rect2Segment (ROI r _ c _) sz (IppiRect x y w h) =
    Segment p1' p2'
        where p1 = Pixel (r+ ti y) (c+ ti x)
              p2 = Pixel (r+ ti h) (c+ ti w)
              [p1',p2'] = pixelsToPoints sz [p1,p2]

-------------------------------------------------------------------

foreign import ccall "initCascade"
    c_initCascade :: CString -> IO (Ptr ())

foreign import ccall "cascadeDetect"
    c_cascadeDetect :: Ptr ()
                 -> Ptr () -> CInt -> CInt -> CInt
                 -> CInt -> CInt -> CInt -> CInt
                 -> CInt -> Ptr CInt -> Ptr IppiRect -> IO ()

cascadeDetect :: Ptr () -> ImageGray -> Int -> [ROI]
cascadeDetect casc imag fmax = unsafePerformIO $ do
    pn <- malloc
    pf <- mallocArray fmax
    app1G (c_cascadeDetect casc) imag (fi fmax) pn pf
    n <- peek pn
    r <- peekArray (ti n) pf
    free pn
    free pf
    return (map (rect2roi (theROI imag)) r)


rect2roi (ROI r _ c _) (IppiRect x y w h) =
    ROI (r+ti y) (r+ti y+ti h-1) (c+ti x) (c+ti x+ti w-1)

cascadeClassifier :: FilePath -> IO (ImageGray -> Int -> [ROI])
cascadeClassifier file = do
    cascade <- newCString file
    c <- c_initCascade cascade
    free cascade
    return (cascadeDetect c)

