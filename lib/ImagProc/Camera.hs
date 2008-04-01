{-# OPTIONS -fffi -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Image acquisition from real cameras and other video sources using MPlayer.

-}
-----------------------------------------------------------------------------

module ImagProc.Camera (
  -- * MPlayer interface
  -- | This camera works with any kind of video source accepted by MPlayer.
  mplayer, mpSize, openYUV4Mpeg,

)where

import ImagProc.Ipp.Core
import Foreign
import Foreign.C.Types (CChar,CUChar)
import Foreign.C.String(newCString)
import Data.IORef
import System.IO
import System
import Data.List(isPrefixOf)

-- | Computes a 4\/3 \'good\' size for both mplayer and IPP. mpSize 20 = 640x480
mpSize :: Int -> Size
mpSize k | k > 0     = Size (k*24) (k*32)
         | otherwise = error "mpSize"


-- | Interface to mplayer (implemented using a pipe and the format yuv4mpeg).
-- It admits the url shortcuts webcam1, webcam2, and firewire,
-- and automatically supplies the required additional parameters.
mplayer :: String               -- ^ any url admitted by mplayer
        -> Size                 -- ^ desired image size (see 'mpsize')
        -> IO (IO ImageYUV)     -- ^ function returning a new frame and camera controller
mplayer url (Size h w) = do

    let fifo = "/tmp/mplayer-fifo"
    system $ "rm -f "++fifo
    system $ "mkfifo "++fifo

    k <- mallocBytes 1
    poke k '\0'         -- essential!!

    let mpcommand = proc w h url++" -vo yuv4mpeg:file="++fifo++" -nosound -slave -loop 0"

    --(i,o,e,p) <- runInteractiveProcess "mplayer" (words mpcommand) Nothing Nothing
    --(i,o,e,p) <- runInteractiveCommand ("mplayer " ++mpcommand)

    putStr "Please abort (Ctrl-C) and check the URL\r"
    system $ "mplayer "++ mpcommand ++" >/dev/null 2>/dev/null &"

    f <- openFile fifo ReadMode

    let find = do
        n <- hGetBuf f k 1
        --print n
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

shsz w h = ":width="++show w++":height="++show h
shsz' w h = ":width=640:height=480" ++ shsc w h
shsc w h = " -vf scale="++show w++":"++show h

-- be careful with the order

proc w h url
    | "swebcam0" `isPrefixOf` url = rep ("swebcam0","tv:// -tv driver=v4l2:device=/dev/video0:fps=15"++shsz w h) url
    | "swebcam1" `isPrefixOf` url = rep ("swebcam1","tv:// -tv driver=v4l2:device=/dev/video1:fps=15"++shsz w h) url
    | "swebcam2" `isPrefixOf` url = rep ("swebcam2","tv:// -tv driver=v4l2:device=/dev/video2:fps=15"++shsz w h) url
    | "swebcam3" `isPrefixOf` url = rep ("swebcam3","tv:// -tv driver=v4l2:device=/dev/video3:fps=15"++shsz w h) url
    | "webcam0" `isPrefixOf` url = rep ("webcam0","tv:// -tv driver=v4l:device=/dev/video0"++shsz' w h) url
    | "webcam1" `isPrefixOf` url = rep ("webcam1","tv:// -tv driver=v4l:device=/dev/video1"++shsz' w h) url
    | "webcam2" `isPrefixOf` url = rep ("webcam2","tv:// -tv driver=v4l:device=/dev/video2"++shsz' w h) url
    | "webcam3" `isPrefixOf` url = rep ("webcam3","tv:// -tv driver=v4l:device=/dev/video3"++shsz' w h) url
    | "webcam" `isPrefixOf` url = rep  ("webcam", "tv:// -tv driver=v4l:device=/dev/video0"++shsz' w h) url
    | "firewire" `isPrefixOf` url = rep ("firewire","/dev/dv1394 -demuxer rawdv -cache 400"++shsc w h) url
    | "s-video-di" `isPrefixOf` url =
            rep ("s-video-di", "tv:// -tv driver=v4l2:device=/dev/video0"++shsz w h++" -vf pp=md") url
    | "s-video" `isPrefixOf` url = rep ("s-video", "tv:// -tv driver=v4l2:device=/dev/video0"++shsz w h) url
    | otherwise = url ++ shsc w h

-- deinterlace -vf pp=md

rep (c,r) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)

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
