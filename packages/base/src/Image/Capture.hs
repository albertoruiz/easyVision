-----------------------------------------------------------------------------
{- |
Module      :  Image.Capture
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Video sources

-}
-----------------------------------------------------------------------------

module Image.Capture(
    gcam,
    readImages, readFolderIM, readFolderMP,
    -- mostRecent, keepAll, parseSize,
    webcam, mplayer
)where

import Image.Core
import Image.Convert(loadRGB)
import Image.Devel(yuv2yuyv,parseSize)
import Image.Capture.UVC(webcam)
import Image.Capture.MPlayer
import System.IO.Unsafe(unsafeInterleaveIO)
import Data.List(isPrefixOf,foldl',tails,findIndex,isInfixOf,isSuffixOf,sort)
import System.Directory(doesFileExist, getDirectoryContents)
import Control.Applicative((<$>))
import System.Environment(getArgs,getEnvironment)
import Control.Concurrent
import Control.Concurrent.MSampleVar
import Data.IORef

import Util.Options
import Util.Misc(debug,errMsg)
import Control.Monad
import Util.LazyIO((>~>),lazyList,Generator)
import Text.Printf
import Data.List.Split(splitOn)


--------------------------------------------------------------------------------

gcam :: Generator ImageYCbCr
gcam = do
    args <- getArgs
    let clean = cleanOpts args
    a <- if null args
           then return "uvc"
           else if null clean
                  then optionString "--source" "uvc"
                  else return (head clean)
    let opt = getSubOption a
        has = hasSubOption a

        dev = "/dev/video" ++ opt "dev" "0"
        sz = parseSize $ opt "size" "640x480"
        fps = read (opt "fps" "30")
        
        url = opt "url" (head (splitOn ":" a))
        
    cam <- if has "uvc"
            then webcam dev sz fps
            else if has "size"
                    then mplayer'' url sz
                    else do
                        mbsize <- askSize url
                        case mbsize of
                            Nothing   -> mplayer'' url sz
                            Just oksz -> mplayer'' url oksz

    if has "drop"
        then mostRecent cam
        else if has "keep"
                then keepAll cam
                else return cam

--------------------------------------------------------------------------------

mplayer'' u s = fmap (fmap (fmap yuv2yuyv)) (mplayer u s)

--------------------------------------------------------------------------------

-- | supply the most recent item
mostRecent :: IO a -> IO (IO a)
mostRecent cam = do
    c <- newEmptySV
    _ <- forkIO $ forever $ cam >>= writeSV c {- >> putStr "." -}
    return $ {- putStrLn "" >> -} readSV c


-- | create a channel and keep all items in the stream
keepAll :: IO a -> IO (IO a)
keepAll cam = do
    c <- newChan
    _ <- forkIO $ forever $ cam >>= writeChan c {- >> putStr "." -}
    return $ {- putStrLn "" >> -} readChan c

----------------------------------------------------------------------

isImage :: FilePath -> Bool
isImage name = any g [".png",".PNG",".jpg",".JPG",".bmp",".BMP",".ppm",".PPM"]
  where
    g e = e `isSuffixOf` name


readFolderIM :: FilePath -> IO [(ImageRGB,String)]
-- ^ read the images in a folder.
readFolderIM path = do
    fs <- sort . filter isImage <$> getDirectoryContents path
    errMsg $ show (length fs) ++ " images in " ++ path
    info        <- getFlag "--read-folder-progress"
    let tot = length fs
        progress k | info = putStrLn $ show k ++"/"++show tot
                   | otherwise = return ()
        f (k,p) = fmap (\x-> (x,p))
            . unsafeInterleaveIO . (\x -> progress k >> loadRGB x)
            . debug "loading" (const p)
            $ path++"/"++p
    mapM f (zip [1::Int ..] fs)


readImages :: [FilePath] -> IO [ImageRGB]
-- ^ lazily read a list of images
readImages fs = do
    let f p = unsafeInterleaveIO . loadRGB
            . debug "loading" (const p)
            $ p
    mapM f fs


readFolderMP :: FilePath -> Maybe Size -> IO [(ImageYCbCr,String)]
-- ^ read the images in a folder (fixed size, using mplayer)
readFolderMP path mbsz = do
    let sz = maybe (Size 600 800) id mbsz -- TO DO: remove fixed size
    fs <- getDirectoryContents path
    let nframes = length (filter isImage fs)
    cam <- mplayer' ("mf://"++path++"/ -benchmark -loop 1") sz
    imgs <- sequence (replicate nframes cam)
    errMsg $ show (length imgs) ++ " images in " ++ path
    return $ zip (map yuv2yuyv imgs) (map show [(1::Int)..])

