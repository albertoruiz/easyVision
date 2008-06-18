-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Util
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Other utilities.

-}
-----------------------------------------------------------------------------

module EasyVision.Util (
    on,
    findSize,
    getCam, numCams,
    getOption, getRawOption,
    maybeOption, getFlag,
    optionalSaver,
    captureGL,
    saveRGB,
    evSize,
    glSize,
    timing,
    debug,
    getAliases
)where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc.ImageProcessing(resize32f,yuvToRGB)
import ImagProc.Camera
import Foreign (touchForeignPtr,castPtr)
import ImagProc.Images
import qualified Data.Map as Map
import System.IO
import System
import Data.List(isPrefixOf,foldl',tails,findIndex)
import Directory(getDirectoryContents,doesFileExist)
import System.CPUTime
import Text.Printf
import Debug.Trace
import Control.Monad(when)
import System.Environment(getArgs,getEnvironment)
import qualified Data.Map as Map

on f g = \x y -> f (g x) (g y)

timing act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    printf "%.3f s CPU\n" $ (fromIntegral (t1 - t0) / (10^12 :: Double)) :: IO ()

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
evSize (GL.Size w h) = Size    (t h) (t w) where t = fromIntegral.toInteger
glSize (Size    h w) = GL.Size (t w) (t h) where t = fromIntegral.toInteger

-- | Writes to file (with automatic name if Nothing) a RGB image in png format.
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

{- | It tries to read an optional image size from command line argument list.
     It admits --rows, --cols, and --size (for 32k 4\/3).
-}
findSize :: IO Size
findSize = do
    args <- getArgs
    let opts = Map.fromList $ zip args (tail args)
        fwd  = Map.findWithDefault
        sz   =  if Map.member "--size" opts
                    then mpSize $ read $ fwd "20" "--size" opts
                    else Size (read $ fwd "480" "--rows" opts)
                              (read $ fwd "640" "--cols" opts) 
    return sz


getCam n sz = do
    args <- cleanOpts `fmap` getArgs
    aliases <- getAliases
    let url = if n < length args
                then args!!n
                else fst (aliases!!n)
    mplayer (expand aliases url) sz


getOption name def = do
    mbopt <- getRawOption name
    let v = case mbopt of
                Nothing -> def
                Just s  -> read s
    return v


getRawOption name = do
    args <- getArgs
    let opts = filter ((name++"=") `isPrefixOf`) args
    return $ case opts of
        [] -> Nothing
        xs -> Just (val (last xs))
  where val = tail . dropWhile (/= '=')


maybeOption name = fmap (fmap read) (getRawOption name)

getFlag name = do
    args <- getArgs
    return (name `elem` args)

-- | Number of explicit camera arguments supplied (automatic alias are not used)
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

optionalSaver sz = do
    filename <- getRawOption "--save"
    limit    <- maybeOption "limit"
    openYUV4Mpeg sz filename limit

----------------------------------------------
