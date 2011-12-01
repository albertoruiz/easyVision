{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.UVC
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional


Interface to uvc cameras stolen from luvcview.

-}
-----------------------------------------------------------------------------

module ImagProc.C.UVC (uvcCamera)
where

import Foreign
import Foreign.C.Types(CChar)
import Foreign.C.String(newCString)
import ImagProc.Ipp hiding (r1,c1,r2,c2)

#ifdef HASUVC

foreign import ccall "openUVC"
    c_openUVC :: Ptr CChar -> CInt -> CInt -> CInt -> IO (Ptr ())

foreign import ccall "grabUVC"
    c_grabUVC :: Ptr () -> Ptr () -> IO ()

uvcCamera :: String -> Size -> Int -> IO (IO ImageYUV)
uvcCamera d (Size h w) fps = do
    dev <- newCString d
    han <- c_openUVC dev (fromIntegral w) (fromIntegral h) (fromIntegral fps)
    return $ do
        Y im <- image (Size h w)
        c_grabUVC han (ptr im)
        return (Y im)

#else

uvcCamera :: String -> Size -> Int -> IO (IO ImageYUV)
uvcCamera d (Size h w) fps = do
    return $ do
        Y im <- image (Size h w)
        -- fill it with something nice...
        return (Y im)
#endif


