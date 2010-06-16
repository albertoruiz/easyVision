{-# LANGUAGE ForeignFunctionInterface #-}

module OpenCV (testOpenCV, testImage8u, testHough, cascadeClassifier) where

import ImagProc.Ipp.Core
import Foreign
import Foreign.C.Types
import Foreign.C.String

testOpenCV :: IO ()
testOpenCV = do
    putStrLn "Hello OpenCV!"
    c_opencvTest


foreign import ccall "opencv_test" c_opencvTest :: IO ()

app1G f (G im) = do
    f
       (ptr im) (fi.step$im) (fi $ width $ isize im) (fi $ height $ isize im)
       (fi $ r1 $ vroi im) (fi $ r2 $ vroi im) (fi.c1.vroi $ im) (fi.c2.vroi$im)


------------------------------------------------------------------

testImage8u :: ImageGray -> IO ()
testImage8u = app1G c_testImage8u

testHough :: ImageGray -> IO ()
testHough = app1G c_houghTest


foreign import ccall "testImage8u"
    c_testImage8u :: Ptr () -> CInt -> CInt -> CInt
                     -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import ccall "houghTest"
    c_houghTest :: Ptr () -> CInt -> CInt -> CInt
                   -> CInt -> CInt -> CInt -> CInt -> IO ()

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
    return (map rect2roi r)


rect2roi (IppiRect x y w h) = ROI (ti y) (ti y+ti h-1) (ti x) (ti x+ti w-1)

cascadeClassifier :: FilePath -> IO (ImageGray -> Int -> [ROI])
cascadeClassifier file = do
    cascade <- newCString file
    c <- c_initCascade cascade
    free cascade
    return (cascadeDetect c)
