{-# OPTIONS -fffi #-} 
 
module Camera where 
 
import Ipp 
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

openCamera device mode (h,w) = do
    cdev <- newCString device
    cam <- openCameraC cdev
    setCameraModeC cam intmode h w
    return (cam,mode,(h,w))
 where intmode = case mode of
                   RGB -> 0
                   Gray -> 1
                   _ -> error "image type not supported by the camera"
    
grab (cam,mode,(h,w)) = do
    pstep <- malloc
    dat <- getFrameC cam pstep
    stepc <- peek pstep
    res <- img mode h w
    copyArray (castPtr $ ptr res) dat (h*stepc)
    return res 

type Camera = (Int,ImageType,(Int,Int))
