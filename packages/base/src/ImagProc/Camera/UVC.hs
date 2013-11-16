{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera.UVC
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional


Interface to uvc cameras stolen from luvcview.

-}
-----------------------------------------------------------------------------

module ImagProc.Camera.UVC (uvcCamera, webcamRGB, webcam, webcamGray)
where

import Foreign
import Foreign.C.Types
import Foreign.C.String(newCString)
import Image.Core hiding (r1,c1,r2,c2)

-- #define HASUVC
#ifdef HASUVC

foreign import ccall "openUVC"
    c_openUVC :: Ptr CChar -> CInt -> CInt -> CInt -> IO (Ptr ())

foreign import ccall "grabUVC"
    c_grabUVC :: CInt -> Ptr () -> Ptr Word8 -> IO CInt

uvcCamera :: String -> Size -> Int -> IO (IO ImageYUV)
uvcCamera d (Size h w) fps = do
    dev <- newCString d
    han <- c_openUVC dev (fromIntegral w) (fromIntegral h) (fromIntegral fps)
    return $ do
        Y im <- image (Size h w)
        ok <- c_grabUVC 2 han (ptr im)
        if ok == 0
          then return (Y im)
          else error "grab error in uvcCamera"

--------------------------------------------------------------------------------

webcamRGB
  :: Int  -- ^ device
  -> Size
  -> Int  -- ^ frame rate
  -> IO (IO (Maybe ImageRGB))
webcamRGB d (Size h w) fps = do
    dev <- newCString ("/dev/video"++show d)
    han <- c_openUVC dev (fromIntegral w) (fromIntegral h) (fromIntegral fps)
    return $ do
        C im <- image (Size h w)
        ok <- c_grabUVC 1 han (ptr im)
        if ok==0
          then return (Just (C im))
          else return Nothing


webcam
  :: Int  -- ^ device
  -> Size
  -> Int  -- ^ frame rate
  -> IO (IO (Maybe ImageYCbCr))
webcam d (Size h w) fps = do
    dev <- newCString ("/dev/video"++show d)
    han <- c_openUVC dev (fromIntegral w) (fromIntegral h) (fromIntegral fps)
    return $ do
        im <- img YCbCr (Size h w)
        ok <- c_grabUVC 0 han (ptr im)
        if ok==0
          then return (Just (Y422 im))
          else return Nothing


webcamGray
  :: Int  -- ^ device
  -> Size
  -> Int  -- ^ frame rate
  -> IO (IO (Maybe ImageGray))
webcamGray d (Size h w) fps = do
    dev <- newCString ("/dev/video"++show d)
    han <- c_openUVC dev (fromIntegral w) (fromIntegral h) (fromIntegral fps)
    return $ do
        G im <- image (Size h w)
        ok <- c_grabUVC 3 han (ptr im)
        if ok==0
          then return (Just (G im))
          else return Nothing

--------------------------------------------------------------------------------

#else

uvcCamera :: String -> Size -> Int -> IO (IO ImageYUV)
uvcCamera _ _ _ = error "uvc camera not available"

webcam :: Int -> Size -> Int -> IO (IO (Maybe ImageRGB))
webcam _ _ _ = return (return Nothing)

#endif

