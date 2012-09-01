{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera.Mplayer
Copyright   :  (c) Alberto Ruiz 2006-2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Image acquisition from real cameras and other video sources using MPlayer.

-}
-----------------------------------------------------------------------------

module ImagProc.Camera.MPlayer (
  -- * MPlayer interface
  -- | This camera works with any kind of video source accepted by MPlayer.
  mplayer, mplayer', mpSize, askSize,
  saveYUV4Mpeg, yuvHeader, openYUV4Mpeg
)where

import ImagProc.Ipp.Core
import Foreign
import Data.IORef
import System.IO
import System.Process(system,readProcessWithExitCode)
import System.Exit
import Data.List(isInfixOf,isPrefixOf)
import Util.Options
import Control.Monad
import Control.Applicative
import Data.Maybe(listToMaybe)

-- | Computes a 4\/3 \'good\' size for both mplayer and IPP. mpSize 20 = 640x480
mpSize :: Int -> Size
mpSize k | k > 0     = Size (k*24) (k*32)
         | otherwise = error "mpSize"


-- | Interface to mplayer (implemented using a pipe and the format yuv4mpeg).
-- It admits the url shortcuts webcam1, webcam2, and firewire,
-- and automatically supplies the required additional parameters.
-- The grab function returns Nothing if there are no remaining frames to read.
mplayer' :: String                       -- ^ any url admitted by mplayer
         -> Size                         -- ^ desired image size (see 'mpsize')
         -> IO (IO (Maybe ImageYUV))     -- ^ function returning a new frame
mplayer' url (Size h w) = do

    verbose <- when <$> getFlag "-v"

    let fifo = "/tmp/mplayer-fifo"
    _ <- system $ "rm -f "++fifo
    _ <- system $ "mkfifo "++fifo

    k <- mallocBytes 1
    poke k '\0'         -- essential!!

    let loop | "-loop" `isInfixOf` url = ""
             | otherwise               = " -loop 0"

        vfscale = " -vf scale=" ++ show w ++ ":" ++ show h

        url' | "-vf" `isInfixOf` url = url
             | otherwise             = url ++ vfscale

        mpcommand =  url'
                  ++ " -vo yuv4mpeg:file="
                  ++ fifo
                  ++ " -nosound -slave"
                  ++ loop

    --(i,o,e,p) <- runInteractiveProcess "mplayer" (words mpcommand) Nothing Nothing
    --(i,o,e,p) <- runInteractiveCommand ("mplayer " ++mpcommand)

    verbose $ do putStrLn mpcommand
                 putStr "Press Ctrl-C and check the URL\r"
    
    _ <- system $ "mplayer "++ mpcommand ++" >/dev/null 2>/dev/null &"

    f <- openFile fifo ReadMode

    let find ss = do
        _n <- hGetBuf f k 1
        --print _n
        v <- peek (castPtr k)
        verbose $ putChar v
        if v=='\n' then return (reverse ss) else find (v:ss)

    info <- find ""
    --putStrLn info
    let [w',h'] = map (read.tail) . take 2 . drop 1 . words $ info
    --print (Size h' w')
    
    _ <- system $ "rm "++fifo

    let grab = do
        Y im <- image (Size h' w')
        let frameSize = w'*h'*3`div`2
        n <- hGetBuf f (castPtr (ptr im)) 6
        if n < 6
            then return Nothing
            else do _ <- hGetBuf f (castPtr (ptr im)) frameSize
                    return (Just (Y im))

    return grab

-- deinterlace -vf pp=md

-- | The same as @mplayer'@, but it returns a grab function which exits the program if
-- there are no remaining frames to read.
mplayer :: String                       -- ^ any url admitted by mplayer
        -> Size                         -- ^ desired image size (see 'mpsize')
        -> IO (IO ImageYUV)             -- ^ function returning a new frame
mplayer url sz = f `fmap` mplayer' url sz where
    f mbcam = mbcam >>= (maybe (exitWith ExitSuccess) return)

------------------------------------------------

askSize :: FilePath -> IO (Maybe Size)
-- ^ check if video exists and return frame size
askSize fname = do
    (_,res,_) <- readProcessWithExitCode "mplayer" [fname, "-nosound", "-vo", "null", "-frames", "1"] ""
    let info = listToMaybe $ filter (isPrefixOf "VIDEO:") (lines res)
    return (f `fmap` info)
  where
    f = h . words . map g . (!!2) . words
    g 'x' = ' '
    g y = y
    h [a,b] = Size (read b) (read a)
    h _ = error "askSize parse error"

------------------------------------------------

saveYUV4Mpeg :: Handle -> ImageYUV -> IO ()
saveYUV4Mpeg handle (Y im) = do
    let Size h w = isize im
    hPutStrLn handle "FRAME"
    hPutBuf handle (castPtr (ptr im)) (w*h*3`div`2)
    hFlush handle
    touchForeignPtr (fptr im)

yuvHeader :: Int -> Int -> FilePath -> IO Handle
yuvHeader h w filename = do
    handle <- openFile filename WriteMode
    hPutStrLn handle $ "YUV4MPEG2 W"++show w++" H"++show h++" F25000000:1000000 Ip A0:0"
    return handle

-- | creates a function to save frames in the mplayer yuv4mpeg format.
openYUV4Mpeg :: Size      -- ^ size of the recorded video
             -> FilePath  -- ^ output filename.
             -> Maybe Int -- ^ optional number of frames to save.
             -> IO (ImageYUV -> IO()) -- ^ returns a function to add a new frame to the video

openYUV4Mpeg (Size h w) filename Nothing = do
    handle <- yuvHeader h w filename
    return (saveYUV4Mpeg handle)

openYUV4Mpeg (Size h w) filename (Just limit) = do
    handle <- yuvHeader h w filename
    framesSaved <- newIORef 0
    return $ \im -> do
        saveYUV4Mpeg handle im
        fs <- readIORef framesSaved
        if fs < limit then writeIORef framesSaved (fs+1)
                      else exitWith ExitSuccess

