-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Camera
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Video sources

-}
-----------------------------------------------------------------------------

module ImagProc.Camera(
    -- * Camera selection
    findSize,
    getCam, numCams,
    getMulticam,
    readFrames,
    readImages, readFolderMP, readFolderIM,
    -- * Video I/O
    writeFrames,
    optionalSaver,
    autoSaver,
    process,
    saveFrame,
    module ImagProc.Camera.MPlayer
)where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Convert(loadRGB)
import ImagProc.Generic(Channels,channels,GImg,toYUV,channelsFromRGB)
import ImagProc.Camera.MPlayer
import System.IO.Unsafe(unsafeInterleaveIO)
import Data.List(isPrefixOf,foldl',tails,findIndex,isInfixOf,isSuffixOf)
import Data.Maybe
import System.Directory(doesFileExist, getDirectoryContents)
import Control.Applicative((<$>))
import System.Environment(getArgs,getEnvironment)
import Control.Concurrent
import Data.IORef
import ImagProc.Camera.UVC
import Util.Options
import Util.Misc(debug)
import Control.Monad
import Util.LazyIO((>~>),grabAll)

-----------------------------------------------------------------------------------

{- | Extracts an optional image size from command line.
     It admits --rows, --cols, and --size (for 32k 4\/3). The default is --size=20 (640x480).
-}
findSize :: IO Size
findSize = do
    okSize <- getFlag "--size"
    mps <- getOption "--size" 20
    r   <- getOption "--rows" 480
    c   <- getOption "--cols" 640
    return $ if okSize
                then mpSize mps
                else Size r c

-- | returns a camera from the n-th user argument
getCam :: Int  -- ^ n-th camera url supplied by the user (or defined in cameras.def)
       -> Size -- ^ image size
       -> IO (IO ImageYUV)
getCam n sz = do
    rawargs <- getArgs
    aliases <- getAliases
    dbg <- when <$> getFlag "-v"
    let args = cleanSingleOpts rawargs
        url = if n < length args
                then args!!n
                else fst (aliases!!n)
        fullUrl = dropWhile (== ' ') $ expand aliases url
        isLive = "--live" `isInfixOf` fullUrl || "--live" `elem` rawargs
        isChan = "--chan" `isInfixOf` fullUrl || "--chan" `elem` rawargs
        clean ws = unwords . filter (not . (`elem` ws)). words
        cleanUrl = clean ["--live"] fullUrl
        uvcdev = "/dev/video" ++ drop 3 cleanUrl
        cam = if "uvc" `isPrefixOf` cleanUrl
                then dbg (putStrLn uvcdev) >> uvcCamera uvcdev sz 30
                else dbg (putStrLn cleanUrl) >> mplayer cleanUrl sz
    if isLive
        then dbg (putStrLn "Live") >> cam >>= live
        else if isChan 
                 then dbg (putStrLn "Channel") >> cam >>= channel
                 else cam
  where
    cleanSingleOpts = filter $ \x -> not ("-" `isPrefixOf` x) || ' ' `elem` x

----------------------------------------------

getMulticam :: Size -> Int -> IO (IO [Channels])
getMulticam sz n = do
    cams <- mapM (flip getCam sz >~> channels) [0..n-1]
    return (sequence cams)


-----------------------------------------------

readFrames :: Int  -- ^ n-th camera url supplied by the user (or defined in cameras.def)
           -> IO [ImageYUV]
-- | returns a lazy list with all the frames produced by an image source.
readFrames = fmap (map fromJust . takeWhile isJust) . readFrames'
  where
    readFrames' n = do
        args <- cleanOpts `fmap` getArgs
        aliases <- getAliases
        sz <- findSize
        let url = if n < length args
                    then args!!n
                    else fst (aliases!!n)
        mplayer' (expand aliases url) sz >>= grabAll


writeFrames :: FilePath -> [ImageYUV] -> IO ()
-- ^ writes a list of frames in a file with the yuv4mpeg format understood by mplayer.
-- If the output filename is /dev/stdout you can pipe the result to mencoder like this:
--
-- ./prog | mencoder - -o result.avi -ovc lavc -fps 125 [other options]
writeFrames _ [] = return ()
writeFrames filename fs@(f0:_) = do
    let sz = size f0
    sv <- openYUV4Mpeg sz filename Nothing
    mapM_ sv fs


-- | Number of explicit camera arguments supplied (automatic aliases from cameras.def are not taken into account).
numCams :: IO Int
numCams = (length . cleanOpts) `fmap` getArgs

getAliases :: IO [(String, String)]
getAliases = do
    env <- getEnvironment
    let ev = lookup "EASYVISION" env
    okHere <- doesFileExist "cameras.def"
    okThere <- case ev of
        Nothing   -> return False
        Just pth -> doesFileExist (pth++"/cameras.def")
    fmap prepareAlias $
        if okHere
                then readFile "cameras.def"
                else if okThere
                        then let Just pth = ev in readFile (pth++"/cameras.def")
                        else return []
  where
    prepareAlias = filter (uncurry (/=)) . map (break (==' ')) . lines

expand :: Eq a => [([a], [a])] -> [a] -> [a]
expand ali s = foldl' (flip replace) s ali
  where
    replace (a,b) c = case findIndex (isPrefixOf a) (tails c) of
        Nothing -> c
        Just i  -> take i c ++ b ++ replace (a,b) (drop (i+length a) c)

----------------------------------------------

optionalSaver :: Size -> IO (ImageYUV -> IO ())
optionalSaver sz = do
    filename <- getRawOption "--save"
    limit    <- maybeOption "--limit"
    case filename of
        Nothing   -> return $ const (return ())
        Just path -> openYUV4Mpeg sz path limit

----------------------------------------------

-- | always saves the frames, in a new file if not given --save=<filename>
autoSaver :: IO (ImageYUV -> IO ())
autoSaver = do
    filename <- findName
    limit    <- maybeOption "--limit"
    svref <- newIORef Nothing
    return $ \im -> do
        mbsv <- readIORef svref
        case mbsv of
            Just sv -> sv im
            Nothing -> do sv <- openYUV4Mpeg (size im) filename limit
                          writeIORef svref (Just sv)
                          sv im            
  where
    findName :: IO String
    findName = do
        argfn <- getRawOption "--save"
        newname <- (++".yuv") `fmap` nextFilename "savedframes"
        return $ case argfn of
                    Nothing -> newname
                    Just filename -> filename

----------------------------------------------


-- | Creates a virtual camera which always supplies the most recent object.
-- When an object is consumed the caller will be blocked until a new object is available 
-- (the same object is not returned multiple times).
-- This is useful for live cameras when processing is slow.
--
-- This virtual camera is automatically added by getCam if the url contains --live
-- (it can be added in cameras.def). Therefore, programs do not need to worry about
-- the difference between live and offline cameras.

live :: IO a -> IO (IO a)
live cam = do
    c <- newEmptySampleVar
    _ <- forkIO $ forever $ cam >>= writeSampleVar c {- >> putStr "." -}
    return $ {- putStrLn "" >> -} readSampleVar c 


channel :: IO a -> IO (IO a)
channel cam = do
    c <- newChan
    _ <- forkIO $ forever $ cam >>= writeChan c {- >> putStr "." -}
    return $ {- putStrLn "" >> -} readChan c 


-----------------------------------------------------------

-- | offline video processing. The input is the camera 0 and the output
--   is given by --save=[saved.yuv]
process :: (GImg pixel a) => ([Channels] -> [a]) -> IO ()
process f = do
    outfile <- optionString "--save" "saved.yuv"
    xs <- readFrames 0
    let ys = map toYUV . f . map channels $ xs
    writeFrames outfile ys

-- | creates a function which saves frames 
saveFrame :: (b -> ImageYUV) -> IO b -> IO (IO b)
saveFrame f cam = do
    filename <- optionString "--save" "saved.yuv"
    sz <- findSize
    sv <- openYUV4Mpeg sz filename Nothing
    return $ do
        x <- cam
        sv (f x)
        return x

----------------------------------------------------------------------
 
isImage :: FilePath -> Bool
isImage name = any g [".png",".jpg",".JPG"]
  where
    g e = e `isSuffixOf` name


readFolderIM :: FilePath -> IO [(Channels,String)]
-- ^ reads a list of images from a folder. Variable size, using imageMagick
readFolderIM path = do
    fs <- filter isImage <$> getDirectoryContents path
    putStrLn $ show (length fs) ++ " images in " ++ path
    let f p = fmap (\x-> (channelsFromRGB x,p))
            . unsafeInterleaveIO . loadRGB
            . debug "loading" (const p)
            $ path++"/"++p
    mapM f fs


readImages :: [FilePath] -> IO [Channels]
-- ^ reads a list of images (variable size, using imageMagick)
readImages fs = do
    let f p = fmap channelsFromRGB
            . unsafeInterleaveIO . loadRGB
            . debug "loading" (const p)
            $ p
    mapM f fs


readFolderMP :: FilePath -> Maybe Size -> IO [(Channels,String)]
-- ^ reads a list of images from a folder. Fixed size using mplayer
readFolderMP path mbsz = do
    let sz = maybe (Size 600 800) id mbsz -- TO DO: remove fixed size
    fs <- getDirectoryContents path
    let nframes = length (filter isImage fs)
    cam <- mplayer ("mf://"++path++"/ -benchmark -loop 1") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show (length imgs) ++ " images in " ++ path
    return $ zip (map channels imgs) (map show [(1::Int)..])


