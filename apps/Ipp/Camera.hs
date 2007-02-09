{-# OPTIONS -fffi -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Camera
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Image acquisition from real cameras and other video sources.

-}
-----------------------------------------------------------------------------

module Ipp.Camera (
  -- * MPlayer interface
  -- | This camera works with any kind of video source accepted by MPlayer.
  mplayer, mpSize,
  -- * Explicit DV decodification
  -- | Wrapper to Pedro E. Lopez de Teruel interface to IEEE1394 cameras and dv videos.
  Camera
, openCamera
, Grabbable(..)
, pause
)where

import Ipp.Core
import Ipp.ImageProcessing (scale8u32f,copy8u,copy8uC3)
import Foreign
import Foreign.C.Types (CChar,CUChar)
import Foreign.C.String(newCString)
import Data.IORef
import System.IO
import System

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
           -> Size        -- ^ desired size of the images (height,width)
           -> IO Camera   -- ^ camera descriptor
openCamera device (s@(Size h w)) = do
    cdev <- newCString device
    cam <- openCameraC cdev
    setCameraModeC cam (intmode Gray) h w
    r <- newIORef CameraDescriptor
        {cid = cam,
                          ctype = Gray,
                          csize = s,
                          cpaused = False,
                          lastGray = undefined,
                          lastRGB = undefined
                          }
    return (CM r)

intmode mode = case mode of
            RGB -> 0
            Gray -> 1
            _ -> error "image type not supported by the camera"

-- | Grabs an image from a camera.
rawGrab:: CameraDescriptor -> IO Img
rawGrab CameraDescriptor {cid = cam, ctype= mode, csize = (Size h w)} = do
    pstep <- malloc
    dat <- getFrameC cam pstep
    stepc <- peek pstep
    res <- img mode (Size h w)
    copyArray (castPtr $ ptr res) dat (h*stepc)
    return res



data CameraDescriptor = CameraDescriptor
    { cid   :: Int
    , ctype :: ImageType
    , csize :: Size
    , cpaused :: Bool
    , lastGray :: ImageGray
    , lastRGB :: ImageRGB
    }

-- | Abstract camera descriptor.
newtype Camera = CM (IORef CameraDescriptor)

class Grabbable a where
    grab :: Camera -> IO a

instance Grabbable ImageGray where
    grab = grabGray

instance Grabbable ImageRGB where
    grab = grabRGB

grabGray :: Camera -> IO ImageGray
grabGray (CM rc) = do
    c <- readIORef rc
    if cpaused c
        then do
            r <- copy8u (lastGray c)
            return r
        else if ctype c == Gray
                then do
                    i <- rawGrab c
                    return (G i)
                else do
                    setCameraModeC (cid c) (intmode Gray) (height (csize c)) (width (csize c))
                    writeIORef rc c {ctype=Gray}
                    i <- grab (CM rc)
                    return i


grabRGB :: Camera -> IO ImageRGB
grabRGB (CM rc) = do
    c <- readIORef rc
    if cpaused c
        then do
            r <- copy8uC3 (lastRGB c)
            return r
        else if ctype c == RGB
                then do
                    i <- rawGrab c
                    return (C i)
                else do
                    setCameraModeC (cid c) (intmode RGB) (height (csize c)) (width (csize c))
                    writeIORef rc c {ctype=RGB}
                    i <- grab (CM rc)
                    return i

-- | Alternatively freezes or unfreezes the camera.
pause :: Camera -> IO ()
pause (CM rc) = do
    c <- readIORef rc
    if cpaused c
        then writeIORef rc c {cpaused = False, ctype = Gray} -- the same mode as the last one
        else do                                              --  in the else branch
            rgb  <- grab (CM rc)
            gray <- grab (CM rc)
            writeIORef rc c {cpaused = True, lastGray = gray, lastRGB = rgb}

--------------------------------------------------------------------------------

-- | Computes a 4\/3 \'good\' size for both mplayer and IPP. mpSize 20 = 640x480
mpSize :: Int -> Size
mpSize k | k > 0     = Size (k*24) (k*32)
         | otherwise = error "mpSize"


-- | Interface to mplayer (implemented using a pipe and the format yuv4mpeg)
mplayer :: String               -- ^ any url admitted by mplayer
        -> Size                 -- ^ desired image size (see 'mpsize')
        -> IO (IO ImageYUV,
               String -> IO ()) -- ^ function returning a new frame and camera controller
mplayer url (Size h w) = do

    let fifo = "/tmp/mplayer-fifo-"
    system $ "mkfifo "++fifo

    k <- mallocBytes 1

    let mpcommand =
             url++" -vo yuv4mpeg:file="++fifo++
             " -vf scale="++show w++":"++show h++" -nosound -slave -loop 0 "++
             "-tv driver=v4l:width="++show w++":height="++show h

    --(i,o,e,p) <- runInteractiveProcess "mplayer" (words mpcommand) Nothing Nothing
    --(i,o,e,p) <- runInteractiveCommand ("mplayer " ++mpcommand)

    system $ "mplayer "++ mpcommand ++" >/dev/null 2>/dev/null &"

    f <- openFile fifo ReadMode

    let find = do
        hGetBuf f k 1
        v <- peek (castPtr k)
        putChar v
        if v=='\n' then return () else find

    find
    system $ "rm "++fifo

    paused    <- newIORef False
    frozen <- newIORef undefined

    let rawgrab = do
        Y im <- image (Size h w)
        --hGetLine f >>= print
        hGetBuf f (castPtr (ptr im)) 6 -- find?
        n <- hGetBuf f (castPtr (ptr im)) (w*h*3`div`2)
        --print n
        return (Y im)

    let control command = do
        case command of
         "pause" -> do modifyIORef paused not
                       p <- readIORef paused
                       if p then rawgrab >>= writeIORef frozen
                            else return ()

    let grab = do
        p <- readIORef paused
        if p then readIORef frozen
             else rawgrab

    return (grab,control)
