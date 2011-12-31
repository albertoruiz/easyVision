{-# LANGUAGE ForeignFunctionInterface,
             MagicHash,
             UnboxedTuples,
             BangPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Convert
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

IO and conversion to matrix.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Convert (
    -- * I
    saveRGB',
    saveGray, loadGray, saveRGB, loadRGB,
    -- * Conversion to Matrix
    img2mat, mat2img
) where

import ImagProc.Ipp.Core
import Foreign hiding (shift)
import Control.Monad(when)
import Control.Arrow((***))
import System.IO
import System.Process
import System.Directory(getDirectoryContents)
import Data.List(isPrefixOf)
import Data.Packed.Development(app1,mat,cmat,createMatrix,MatrixOrder(..))
import Numeric.LinearAlgebra(Matrix,rows,cols)
import Util.Options(getOption)


-- | Writes to a file (with automatic name if Nothing) a RGB image in png format.
-- (uses imagemagick' convert.)
saveRGB' :: Maybe FilePath -> ImageRGB -> IO ()
saveRGB' (Just filename) (C im) = do
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

saveRGB' Nothing im = do
    let name = "screenshot"
    fs <- getDirectoryContents "."
    let n = 1+ length (filter (name `isPrefixOf`) fs)
        sn = show n
        k = 3 - length sn
        shj = replicate k '0' ++ sn
    saveRGB' (Just (name ++"-"++ shj)) im

----------------------------------------------------------------------

mat2img :: Matrix Float -> ImageFloat
mat2img m = unsafePerformIO $ do
    (F im) <- image (Size (rows m) (cols m))
    let (ps,c) = roiPtrs im
        f pS pD k = copyBytes pD (plusPtr pS (c*4*k)) (c*4)
        g r _ p = do
            sequence_ $ zipWith (f p) ps [0..fromIntegral r-1]
            return 0 
    app1 g mat (cmat m) "mat2img"
    return (F im)


img2mat :: ImageFloat -> Matrix Float
img2mat (F im) = unsafePerformIO $ do
    let Size r _ = roiSize (vroi im)
        (ps,c) = roiPtrs im
        f pD pS k = copyBytes (plusPtr pD (c*4*k)) pS (c*4)
        g r _ p = do
            sequence_ $ zipWith (f p) ps [0..fromIntegral r-1]
            return 0 
    m <- createMatrix RowMajor r c
    app1 g mat (cmat m) "img2mat" >> return 0 // checkIPP "img2mat" [im]
    return m

----------------------------------------------------------------------

-- | Save the ROI of a 8u image to a file.
-- It uses imagemagick' convert. The file format is given by the extension.
saveRGB :: FilePath -> ImageRGB -> IO ()
saveRGB filename (C im) = do
    handle <- openFile (filename++".rgb") WriteMode
    let (ps,c) = roiPtrs im
        f p = hPutBuf handle p (c*3)
        Size h w = roiSize (vroi im)
    mapM_ f ps
    hClose handle
    touchForeignPtr (fptr im)
    system $ "convert -size "++show w++"x"++show h++" -depth 8 rgb:"
             ++(filename++".rgb ")++filename
    system $ "rm "++(filename++".rgb")
    return ()

----------------------------------------------------------------------

-- | Save the ROI of a 8u image to a file.
-- It uses imagemagick' convert. The file format is given by the extension.
saveGray :: FilePath -> ImageGray -> IO ()
saveGray filename (G im) = do
    handle <- openFile (filename++".8u") WriteMode
    let (ps,c) = roiPtrs im
        f p = hPutBuf handle p c
        Size h w = roiSize (vroi im)
    mapM_ f ps
    hClose handle
    touchForeignPtr (fptr im)
    system $ "convert -size "++show w++"x"++show h++" -depth 8 gray:"
             ++(filename++".8u ")++filename
    system $ "rm "++(filename++".8u")
    return ()

----------------------------------------------------------------------

-- | Load an image using imagemagick's convert.
loadGray :: FilePath -> IO ImageGray
loadGray filename = do
    Size h w <- getSize filename
    let fname = fixSpaces filename
    mh <- getOption "--maxHeight" h
    let (h',w') = fixSizes mh h w
    system $ "convert -resize "++show w' ++"x"++show h'++"! "++fname++" -depth 8 gray:"
             ++(fname++".8u ")
    handle <- openFile (filename++".8u") ReadMode
    G im <- image (Size h' w')
    let (ps,c) = roiPtrs im
        f p = hGetBuf handle p c
    mapM_ f ps
    hClose handle
    touchForeignPtr (fptr im)
    system $ "rm "++(fname++".8u")
    return (G im)

-- | Load an image using imagemagick's convert.
loadRGB :: FilePath -> IO ImageRGB
loadRGB filename = do
    Size h w <- getSize filename
    let fname = fixSpaces filename
    mh <- getOption "--maxHeight" h
    let (h',w') = fixSizes mh h w
    -- print (h,w)
    -- print (h',w')
    --print filename
    system $ "convert -resize "++show w' ++"x"++show h'++"! "++fname++" -depth 8 rgb:"
             ++(fname++".rgb")
    handle <- openFile (filename++".rgb") ReadMode
    C im <- image (Size h' w')
    let (ps,c) = roiPtrs im
        f p = hGetBuf handle p (c*3)
    mapM_ f ps
    hClose handle
    touchForeignPtr (fptr im)
    system $ "rm "++(fname++".rgb")
    return (C im)

fixSizes mh h w = (mkEven *** mkEven) $ if h > mh then (mh, (mh*w) `div` h) else (h,w)
  where
    mkEven n = ((n+1) `div` 2) * 2

fixSpaces = concatMap f
  where
    f ' ' = "\\ "
    f x = [x]

getSize :: FilePath -> IO Size
getSize imagfile = do
    s <- readProcess "identify" [imagfile] ""
    return (g $ words $ map f $ reverse (words s)!!6)
  where
    f 'x' = ' '
    f a = a
    g [w,h] = Size (read h) (read w)

