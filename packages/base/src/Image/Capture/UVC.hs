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

module Image.Capture.UVC (webcam)
where

import Foreign
import Foreign.C.Types
import Foreign.C.String(newCString)
import Image.Devel
import Control.Applicative((<$>))

-- #define HASUVC
#ifdef HASUVC

foreign import ccall "openUVC"
    c_openUVC :: Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr ())

foreign import ccall "grabUVC"
    c_grabUVC :: CInt -> Ptr () -> Ptr Word8 -> IO CInt


webcam
  :: String  -- ^ device
  -> Size    -- ^ requested size
  -> Int     -- ^ frame rate
  -> IO (IO (Maybe ImageYCbCr))
webcam d (Size h w) fps = do
    dev <- newCString d
    pw <- new (fi w)
    ph <- new (fi h)
    pf <- new (fi fps)
    han <- c_openUVC dev pw ph pf
    tw <- ti <$> peek pw
    th <- ti <$> peek ph
    mapM_ free [pw,ph,pf]
    return $ do
        im <- newImage undefined (Size th tw)
        ok <- c_grabUVC 0 han (starting im)
        if ok==0
          then return (Just im)
          else return Nothing

#else

webcam :: Int -> Size -> Int -> IO (IO (Maybe ImageYCbCr))
webcam _ _ _ = return (return Nothing)

#endif

