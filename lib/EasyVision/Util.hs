-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Util
Copyright   :  (c) Alberto Ruiz 2006-8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

General utilities.

-}
-----------------------------------------------------------------------------

module EasyVision.Util(
    -- * Command line options
    getRawOption, getOption, optionString,
    getFlag, hasValue, maybeOption,
    findSize,
    -- * Camera selection
    getCam, numCams,
    readFrames,
    -- * Video output
    writeFrames,
    optionalSaver,
    -- * Misc graphics utilities
    captureGL,
    saveRGB,
    evSize,
    glSize,
    -- * Debug
    timing,
    debug,
    -- * Other
    on,
    lazyRead
)where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc.ImageProcessing(resize32f,yuvToRGB)
import ImagProc.Camera
import Foreign (touchForeignPtr,castPtr)
import ImagProc.Images
import System.IO
import System.IO.Unsafe(unsafeInterleaveIO)
import System
import Data.List(isPrefixOf,foldl',tails,findIndex)
import Directory(getDirectoryContents,doesFileExist)
import System.CPUTime
import Text.Printf
import Debug.Trace
import Control.Monad
import System.Environment(getArgs,getEnvironment)
import Data.Function(on)

timing :: IO a -> IO a
timing act = do
    t0 <- getCPUTime
    r <- act
    t1 <- getCPUTime
    printf "%4.0f ms CPU\n" $ (fromIntegral (t1 - t0) / (10^9 :: Double))
    return r

debug :: Show x => x -> x
debug x = trace (show x) x

-- | captures the contents of the current opengl window (very slow)
captureGL :: IO ImageRGB
captureGL = do
    sz <- get windowSize
    img <- image (evSize sz)
    let C (Img {ptr = p, fptr = f}) = img
    when (width (size img) `rem` 32 /= 0) $ putStrLn "Warning, captureGL with wrong padding"
    readPixels (Position 0 0) sz (PixelData GL.RGB UnsignedByte p)
    touchForeignPtr f
    return img

-- we should use only one size type
-- | converts an OpenGL Size into a 'Size'
evSize :: GL.Size -> Size
evSize (GL.Size w h) = Size    (t h) (t w) where t = fromIntegral.toInteger

-- | converts a 'Size' into an OpenGL Size.
glSize :: Size -> GL.Size
glSize (Size    h w) = GL.Size (t w) (t h) where t = fromIntegral.toInteger

-- | Writes to a file (with automatic name if Nothing) a RGB image in png format.
-- (uses imagemagick' convert.)
saveRGB :: Maybe FilePath -> ImageRGB -> IO ()
saveRGB (Just filename) (C im) = do
    handle <- openFile (filename++".rgb") WriteMode
    let Size h w = isize im
    when (w`rem` 32 /= 0) $ putStrLn "Warning, saveRGB with wrong padding"
    hPutBuf handle (castPtr (ptr im)) (w*h*3)
    hClose handle
    touchForeignPtr (fptr im)
    system $ "convert -flip -size "++show w++"x"++show h++" -depth 8 rgb:"
             ++(filename++".rgb ")++(filename++".png")
    system $ "rm "++(filename++".rgb")
    return ()

saveRGB Nothing im = do
    let name = "screenshot"
    fs <- getDirectoryContents "."
    let n = 1+ length (filter (name `isPrefixOf`) fs)
        sn = show n
        k = 3 - length sn
        shj = replicate k '0' ++ sn
    saveRGB (Just (name ++"-"++ shj)) im

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
    mplayer (expand aliases url) sz

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
readFrames n = fmap mkList (readFrames' n) where
    mkList (Just a: rest) = a : mkList rest
    mkList (Nothing:_)    = []

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
        Just path = ev
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
