{-# OPTIONS -fffi -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Image acquisition from real cameras and other video sources.

-}
-----------------------------------------------------------------------------

module ImagProc.Camera (
  -- * MPlayer interface
  -- | This camera works with any kind of video source accepted by MPlayer.
  mplayer, mpSize, openYUV4Mpeg,
  -- * Camera combinators
  -- | The following combinators create cameras from other cameras
  withPause,
  virtualCamera,
)where

import ImagProc.Ipp.Core
import ImagProc.ImageProcessing (scale8u32f,copy8u,copy8uC3)
import Foreign
import Foreign.C.Types (CChar,CUChar)
import Foreign.C.String(newCString)
import Data.IORef
import System.IO
import System
import System.IO.Unsafe(unsafeInterleaveIO)


-- | Computes a 4\/3 \'good\' size for both mplayer and IPP. mpSize 20 = 640x480
mpSize :: Int -> Size
mpSize k | k > 0     = Size (k*24) (k*32)
         | otherwise = error "mpSize"


-- | Interface to mplayer (implemented using a pipe and the format yuv4mpeg)
mplayer :: String               -- ^ any url admitted by mplayer
        -> Size                 -- ^ desired image size (see 'mpsize')
        -> IO (IO ImageYUV)          -- ^ function returning a new frame and camera controller
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

    let grab = do
        Y im <- image (Size h w)
        --hGetLine f >>= print
        hGetBuf f (castPtr (ptr im)) 6 -- find?
        n <- hGetBuf f (castPtr (ptr im)) (w*h*3`div`2)
        --print n
        return (Y im)

    return grab

------------------------------------------------

saveYUV4Mpeg handle (Y im) = do
    let Size h w = isize im
    hPutStrLn handle "FRAME"
    hPutBuf handle (castPtr (ptr im)) (w*h*3`div`2)
    hFlush handle
    touchForeignPtr (fptr im)


yuvHeader h w filename = do
    handle <- openFile filename WriteMode
    hPutStrLn handle $ "YUV4MPEG2 W"++show w++" H"++show h++" F25000000:1000000 Ip A0:0"
    return handle

-- | creates a function to save frames in the mplayer yuv4mpeg format
openYUV4Mpeg :: Size -> Maybe FilePath -> Maybe Int -> IO (ImageYUV -> IO())

openYUV4Mpeg _ Nothing _ = return $ const (return ())

openYUV4Mpeg (Size h w) (Just filename) Nothing = do
    handle <- yuvHeader h w filename
    return (saveYUV4Mpeg handle)

openYUV4Mpeg (Size h w) (Just filename) (Just limit) = do
    handle <- yuvHeader h w filename
    framesSaved <- newIORef 0
    return $ \im -> do
        saveYUV4Mpeg handle im
        fs <- readIORef framesSaved
        if fs < limit then writeIORef framesSaved (fs+1)
                      else exitWith ExitSuccess

-------------------------------------------------

{- | Adds a pause control to a camera. Commands:

    \"pause\" -> toggles the pause state

    \"step\"  -> toggles the frame by frame state (the next frame is obtained by \"pause\")

-}
withPause :: IO a                       -- ^ original camera
          -> IO (IO a, String -> IO ()) -- ^ camera and controller
withPause grab = do
    paused <- newIORef False
    frozen <- newIORef undefined
    step   <- newIORef False
    

    let control command = do
        case command of
         "pause" -> do modifyIORef paused not
                       p <- readIORef paused
                       if p then grab >>= writeIORef frozen
                            else return ()
         "step"   -> do modifyIORef step not

    let virtual = do
        s <- readIORef step
        p <- readIORef paused
        if not s && p
             then readIORef frozen
             else 
                if s then if p then readIORef frozen
                               else do writeIORef paused True
                                       grab >>= writeIORef frozen
                                       readIORef frozen
             else grab

    return (virtual,control)

-----------------------------------------------

createGrab l = do
    pl <- newIORef l
    return $ do
        h:t <- readIORef pl
        writeIORef pl t
        return h

grabAll grab = do
    im <- grab
    rest <- unsafeInterleaveIO (grabAll grab)
    return (im:rest)

-- | Creates a virtual camera by some desired processing of the infinite list of images produced by another camera.
virtualCamera :: ([a]-> IO [b]) -> IO a -> IO (IO b)
virtualCamera filt grab = grabAll grab >>= filt >>= createGrab