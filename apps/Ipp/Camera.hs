{-# OPTIONS -fffi #-} 

-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Camera
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Wrapper to Pedro E. Lopez de Teruel interface to IEEE1394 cameras and dv videos.

-}
-----------------------------------------------------------------------------

module Ipp.Camera (
  Camera
, openCamera
, grab
)where

import Ipp.Core
import Foreign
import Foreign.C.Types
import Foreign.C.String(newCString)

------------------------------------------------------      
foreign import ccall "auxIpp.h mycvOpenCamera"
     openCameraC :: Ptr CChar -> IO Int

foreign import ccall "auxIpp.h mycvSetModeCamera"
     setCameraModeC :: Int -> Int -> Int -> Int -> IO()

foreign import ccall "auxIpp.h mycvGetFrameCamera"
     getFrameC :: Int -> Ptr Int -> IO (Ptr CChar)
-------------------------------------------------------

-- | Opens a camera.
openCamera :: String      -- ^ path to the device. A live camera, (e.g \/dev\/dv1394) or a raw file.dv.
           -> ImageType   -- ^ type of image to grab
           -> (Int,Int)   -- ^ desired size of the images (height,width)
           -> IO Camera   -- ^ camera descriptor
openCamera device mode (h,w) = do
    cdev <- newCString device
    cam <- openCameraC cdev
    setCameraModeC cam intmode h w
    return (cam,mode,(h,w))
 where intmode = case mode of
                   RGB -> 0
                   Gray -> 1
                   _ -> error "image type not supported by the camera"

-- | Grabs an image from a camera.
grab:: Camera -> IO Img
grab (cam,mode,(h,w)) = do
    pstep <- malloc
    dat <- getFrameC cam pstep
    stepc <- peek pstep
    res <- img mode h w
    copyArray (castPtr $ ptr res) dat (h*stepc)
    return res 

-- | Camera descriptor (device id, imagetype, (height,width)).
type Camera = (Int,ImageType,(Int,Int))
