-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Util
Copyright   :  (c) Alberto Ruiz 2006-8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

General utilities.

-}
-----------------------------------------------------------------------------

module ImagProc.Util(
    -- * Combinators
    virtualCamera, (~~>), (~>), (>~~>), (>~>),
    -- * Command line options
    getRawOption, getOption, optionString,
    getFlag, hasValue, maybeOption,
    findSize,
    -- * Camera selection
    camera,
    getCam, numCams,
    getMulticam,
    readFrames,
    -- * Video output
    writeFrames,
    optionalSaver,
    saveRGB,
    process,
    saveFrame,
    -- * Debug
    timing,
    debug,
    -- * Other
    on,
    lazyRead
)where

import ImagProc.Ipp.Core
import ImagProc.Generic(Channels,channels,GImg,toYUV)
import ImagProc.Camera
import Foreign (touchForeignPtr,castPtr)
import ImagProc.Images
import System.IO
import System.IO.Unsafe(unsafeInterleaveIO)
import System.Process
import Data.List(isPrefixOf,foldl',tails,findIndex,isInfixOf)
import Data.Maybe
import System.Directory(getDirectoryContents,doesFileExist)
import System.CPUTime
import Text.Printf
import Debug.Trace
import Control.Monad
import System.Environment(getArgs,getEnvironment)
import Data.Function(on)
import Control.Concurrent
import Data.IORef
import ImagProc.C.UVC

timing :: IO a -> IO a
timing act = do
    t0 <- getCPUTime
    r <- act
    t1 <- getCPUTime
    printf "%4.0f ms CPU\n" $ (fromIntegral (t1 - t0) / (10^9 :: Double))
    return r

debug :: Show x => x -> x
debug x = trace (show x) x

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
    args <- cleanOpts `fmap` getArgs
    aliases <- getAliases
    let url = if n < length args
                then args!!n
                else fst (aliases!!n)
        fullUrl = expand aliases url
        isLive = "--live" `isInfixOf` fullUrl
    if "uvc" `isPrefixOf` fullUrl
        then uvcCamera ("/dev/video" ++ drop 3 fullUrl) sz 30 >>= live
        else
            if isLive
                then putStrLn "(Live) " >> mplayer (clean ["--live"] fullUrl) sz >>= live
                else mplayer fullUrl sz

clean ws = unwords . filter (not . (`elem` ws)). words

----------------------------------------------

getMulticam :: Size -> Int -> IO (IO [Channels])
getMulticam sz n = do
    cams <- mapM (flip getCam sz >~> channels) [0..n-1]
    return (sequence cams)

-----------------------------------------------

lazyRead :: IO a -> IO [a]
lazyRead get = do
    a  <- get
    as <- unsafeInterleaveIO (lazyRead get)
    return (a:as)

-----------------------------------------------

-- | returns a lazy list with all the frames produced by an image source.
readFrames :: Int  -- ^ n-th camera url supplied by the user (or defined in cameras.def)
           -> IO [ImageYUV]
readFrames n = fmap (map fromJust . takeWhile isJust) (readFrames' n)

readFrames' n = do
    args <- cleanOpts `fmap` getArgs
    aliases <- getAliases
    sz <- findSize
    let url = if n < length args
                then args!!n
                else fst (aliases!!n)
    mplayer' (expand aliases url) sz >>= lazyRead


-- | writes a list of frames in a file with the yuv4mpeg format understood by mplayer.
-- If the output filename is /dev/stdout you can pipe the result to mencoder like this:
--
-- ./prog | mencoder - -o result.avi -ovc lavc -fps 125 [other options]
writeFrames :: FilePath -> [ImageYUV] -> IO ()
writeFrames filename fs@(f0:_) = do
    let sz = size f0
    sv <- openYUV4Mpeg sz filename Nothing
    mapM_ sv fs

-- | extracts an option from the command line. From --alpha=1.5 we get 1.5::Double.
getOption :: Read a
          => String  -- ^ option name (e.g. \"--alpha\")
          -> a       -- ^ default value
          -> IO a    -- ^ result
getOption name def = do
    mbopt <- getRawOption name
    let v = case mbopt of
                Nothing -> def
                Just s  -> read s
    return v

-- | searches for an optional argument
getRawOption :: String -- ^ option name
             -> IO (Maybe String)
getRawOption name = do
    args <- getArgs
    let opts = filter ((name++"=") `isPrefixOf`) args
    return $ case opts of
        [] -> Nothing
        xs -> Just (val (last xs))
  where val = tail . dropWhile (/= '=')


maybeOption name = fmap (fmap read) (getRawOption name)

-- | checks if --option=something has been given in the command line.
hasValue :: String -- ^ option name, including the dashes (e.g. \"--opt\").
         -> IO Bool
hasValue name = do
    x <- getRawOption name
    return $ case x of
        Nothing -> False
        Just _  -> True

-- | checks if --option or --option=something has been given in the command line.
getFlag :: String -- ^ option name, including the dashes (e.g. \"--opt\").
        -> IO Bool
getFlag name = do
    args <- getArgs
    return (any (isPrefixOf name) args)

-- | Special version of 'getOption' for strings, without the need of the quotes required by @read@.
optionString :: String -- ^ option name
             -> String -- ^ default value
             -> IO String
optionString name def = fmap (maybe def id) (getRawOption name)


-- | Number of explicit camera arguments supplied (automatic aliases from cameras.def are not taken into account).
numCams :: IO Int
numCams = (length . cleanOpts) `fmap` getArgs

getAliases = do
    env <- getEnvironment
    let ev = lookup "EASYVISION" env
        Just path' = ev
        path = path' ++ "/"
    okHere <- doesFileExist "cameras.def"
    okThere <- case ev of
        Nothing   -> return False
        Just path -> doesFileExist (path++"cameras.def")
    fmap prepareAlias $
        if okHere
                then readFile "cameras.def"
                else if okThere
                        then let Just path = ev in readFile (path++"cameras.def")
                        else return []

cleanOpts = filter (not . isPrefixOf "-")

prepareAlias = filter (uncurry (/=)) . map (break (==' ')) . lines

expand ali s = foldl' (flip replace) s ali

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
    forkIO $ forever $ cam >>= writeSampleVar c
    return $ readSampleVar c

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
virtualCamera :: ([a]-> [b]) -> IO a -> IO (IO b)
virtualCamera filt grab = filt `fmap` grabAll grab >>= createGrab

-- | shortcut for @>>= virtualCamera f@
(~~>) :: IO (IO a) -> ([a]-> [b]) -> IO (IO b)
infixl 1 ~~>
gencam ~~> f = (gencam >>= virtualCamera f)

-- | shortcut for @>>= virtualCamera (map f)@, or equivalently, @>>= return . fmap f@.
(~>) :: IO (IO a) -> (a-> b) -> IO (IO b)
infixl 1 ~>
gencam ~> f = gencam >>= return . fmap f

-- | composition version of @~>@
(>~>) :: (t -> IO (IO a)) -> (a -> b) -> t -> IO (IO b)
infixr 2 >~>
f >~> g = \x -> f x ~> g

-- | composition version of @~~>@
(>~~>) :: (t -> IO (IO a)) -> ([a] -> [b]) -> t -> IO (IO b)
infixr 2 >~~>
f >~~> g = \x -> f x ~~> g

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

-- | returns the camera 0
camera :: IO (IO Channels)
camera = findSize >>= getCam 0 ~> channels

