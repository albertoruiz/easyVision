-----------------------------------------------------------------------------
{- |
Module      :  Image.Camera
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Video sources

-}
-----------------------------------------------------------------------------

module Image.OldCamera(
    -- * Camera selection
    findSize,
    getCam, numCams,
    getCams, getMulticam,
    readFrames,
    -- * Video I/O
    writeFrames,
    optionalSaver,
    autoSaver,
    saveFrame
)where

import Image.Core
import Image.Base
import Image.Convert(loadRGB)
import ImagProc.Simple(yuv2yuyv)
import ImagProc.Camera.MPlayer
import System.IO.Unsafe(unsafeInterleaveIO)
import Data.List(isPrefixOf,foldl',tails,findIndex,isInfixOf,isSuffixOf,sort)
import System.Directory(doesFileExist, getDirectoryContents)
import Control.Applicative((<$>))
import System.Environment(getArgs,getEnvironment)
import Control.Concurrent
import Data.IORef
import ImagProc.Camera.UVC
import Util.Options
import Util.Misc(debug,errMsg)
import Control.Monad
import Util.LazyIO((>~>),lazyList,Generator)
import Text.Printf
import Data.List.Split(splitOn)

import Image.Camera


{- | Extracts an optional image size from command line.
     It admits --rows, --cols, and --size (for 32k 4\/3). The default is --size=20 (640x480).
-}
findSize :: IO Size
findSize = do
    okSize <- getFlag "--size"
    mps <- optionString "--size" "20"
    r   <- getOption "--rows" 480
    c   <- getOption "--cols" 640
    return $ if okSize
                then parseSize mps
                else Size r c


-- | returns a camera from the n-th user argument
getCam :: Int  -- ^ n-th camera url supplied by the user (or defined in cameras.def)
       -> Size -- ^ image size
       -> Generator ImageYCbCr
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
    uvcdev <- getOption "--uvc" "/dev/video0"
    isuvc <- getFlag "--uvc"
    let cam = if isuvc || null rawargs
                then webcam uvcdev sz 30
                else do dbg (putStrLn cleanUrl)
                        gsz <- askSize cleanUrl
                        def <- or `fmap` mapM hasValue ["--size", "--rows", "--cols"]
                        case gsz of
                            Nothing -> error $ cleanUrl ++ " not found!"
                            Just isz -> if def
                                            then mplayer'' cleanUrl sz
                                            else mplayer'' cleanUrl isz

    if isLive
        then dbg (putStrLn "Live") >> cam >>= mostRecent
        else if isChan
                 then dbg (putStrLn "Channel") >> cam >>= keepAll
       
                 else cam
  where
    cleanSingleOpts = filter $ \x -> not ("-" `isPrefixOf` x) || ' ' `elem` x

mplayer'' u s = fmap (fmap (fmap yuv2yuyv)) (mplayer' u s)

----------------------------------------------

getCams :: IO [IO (Maybe ImageYCbCr)]
getCams = do
    n <- numCams
    sz <- findSize
    cams <- mapM (flip getCam sz) [0..n-1]
    return cams


getMulticam :: Size -> Int -> Generator [ImageYCbCr]
getMulticam sz n = do
    cams <- mapM (flip getCam sz) [0..n-1]
    return (fmap sequence $ sequence cams)


-----------------------------------------------

readFrames :: Int  -- ^ n-th camera url supplied by the user (or defined in cameras.def)
           -> IO [ImageYUV]
-- | returns a lazy list with all the frames produced by an image source.
readFrames n = do
    args <- cleanOpts `fmap` getArgs
    aliases <- getAliases
    sz <- findSize
    let url = if n < length args
                then args!!n
                else fst (aliases!!n)
    mplayer' (expand aliases url) sz >>= lazyList


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

