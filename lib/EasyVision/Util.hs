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
    findSize,
    captureGL,
    saveRGB,
    evSize,
    glSize,
    timing,
    debug
)where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc.ImageProcessing(resize32f,yuvToRGB)
import ImagProc.Camera(mpSize)
import Foreign (touchForeignPtr,castPtr)
import ImagProc.Images
import qualified Data.Map as Map
import System.IO
import System
import Data.List(isPrefixOf)
import Directory(getDirectoryContents)
import System.CPUTime
import Text.Printf
import Debug.Trace
import Control.Monad(when)

timing act = do
    t0 <- getCPUTime
    act
    t1 <- getCPUTime
    printf "%.2f CPU seconds\n" $ (fromIntegral ((t1 - t0) `div` (10^10)) / 100 :: Double)

debug x = trace (show x) x

{- | It tries to read an optional image size from command line argument list.
     It admits --rows, --cols, and --size (for 32k 4\/3).
-}
findSize :: [String] -> Size
findSize args = if Map.member "--size" opts
                    then mpSize $ read $ fwd "20" "--size" opts
                    else Size (read $ fwd "480" "--rows" opts)
                              (read $ fwd "640" "--cols" opts)
    where opts = Map.fromList $ zip args (tail args)
          fwd  = Map.findWithDefault

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
