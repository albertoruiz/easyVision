{-# LANGUAGE ForeignFunctionInterface,
             MagicHash,
             UnboxedTuples,
             BangPatterns #-}

{-# LANGUAGE RecordWildCards #-}


-----------------------------------------------------------------------------
{- |
Module      :  Image.Convert
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

IO and conversion to matrix.

-}
-----------------------------------------------------------------------------


module Image.Convert (
    -- * IO
    saveRGB',
    saveGray, loadGray, saveRGB, loadRGB,
    loadRawPPM,
    savePPM,
    -- * Conversion to Matrix
    img2mat, mat2img
) where

import Image.Devel
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(when)
import Control.Arrow((***))
import Control.Applicative
import System.IO
import System.Process
import System.Directory(getDirectoryContents,doesFileExist)
import Data.List(isPrefixOf,isSuffixOf,intercalate)
import Data.Packed.Development(app1,mat,cmat,createMatrix,MatrixOrder(..))
import Numeric.LinearAlgebra(Matrix,rows,cols)
import Util.Options(getOption)
import Text.Printf
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Util.Misc(formattedTime)
import Image.ROI(roiSize)

-- | Writes to a file (with automatic name if Nothing) a RGB image in png format.
-- (uses imagemagick' convert.)
saveRGB' :: Maybe FilePath -> ImageRGB -> IO ()
saveRGB' (Just filename) im = do
    handle <- openFile (filename++".rgb") WriteMode
    putStrLn $ "Writing result file: " ++ filename ++ ".png"
    let Size h w = size im
    when (w`rem` 32 /= 0) $ putStrLn "Warning, saveRGB with wrong padding"
    withImage im $ do
        hPutBuf handle (starting im) (w*h*3)
        hClose handle
    _ <- system $ "convert -flip -size " ++ show w ++ "x" ++ show h ++ " -depth 8 rgb:"
             ++ (filename++".rgb ") ++ (filename++".png")
    _ <- system $ "rm "++(filename++".rgb")
    return ()

saveRGB' Nothing im = do
      fn <- genName "." "screenshot" ".png"
      saveRGB' (Just fn) im

-- | Generate the next in a sequence of names numbered with suffix "-001" upwards.
-- | Will not produce the name of an existing file. Not completely safe,
-- | since a name and not an opened handle is generated.
genName dir baseName suffix = do
    fis <- getDirectoryContents dir
    let num = length (filter (baseName `isPrefixOf`) fis) + 1
        name n = baseName ++ "-" ++ (printf "%03d" n)
        okNum n = do let nm = name n
                     isFile <- doesFileExist (nm ++ suffix)
                     if isFile then do {putStrLn $ nm ++ suffix ++  " already exists."; okNum (n + 1) }
                      else return n
    goodnum <- okNum num
    return $ name goodnum

----------------------------------------------------------------------


mat2img :: Matrix Float -> ImageFloat
mat2img m = unsafePerformIO $ do
    im <- newImage undefined (Size (rows m) (cols m))
    let (ps,c) = rowPtrs im
        f pS pD k = copyBytes pD (plusPtr pS (c*4*k)) (c*4)
        g r _ p = do
            sequence_ $ zipWith (f p) ps [0..fromIntegral r-1]
            return 0
    app1 g mat (cmat m) "mat2img"
    return im


img2mat :: ImageFloat -> Matrix Float
img2mat im = unsafePerformIO $ do
    let Size r _ = roiSize (roi im)
        (ps,c) = rowPtrs im
        f pD pS k = copyBytes (plusPtr pD (c*4*k)) pS (c*4)
        g r _ p = do
            sequence_ $ zipWith (f p) ps [0..fromIntegral r-1]
            return 0
    m <- createMatrix RowMajor r c
    withImage im $ do
        app1 g mat (cmat m) "img2mat" >> return 0 // checkFFI "img2mat"
    return m

----------------------------------------------------------------------

-- | Save the ROI of a 8u image to a file.
-- It uses imagemagick' convert. The file format is given by the extension.
saveRGB :: FilePath -> ImageRGB -> IO ()
saveRGB filename im = do
    handle <- openFile (filename++".rgb") WriteMode
    let (ps,c) = rowPtrs im
        f p = hPutBuf handle p (c*3)
        Size h w = roiSize (roi im)
    withImage im $ do
        mapM_ f ps
        hClose handle
    _ <- system $ "convert -size "++show w++"x"++show h++" -depth 8 rgb:"
             ++(filename++".rgb ")++filename
    _ <- system $ "rm "++(filename++".rgb")
    return ()

----------------------------------------------------------------------

-- | Save the ROI of a 8u image to a file.
-- It uses imagemagick' convert. The file format is given by the extension.
saveGray :: FilePath -> ImageGray -> IO ()
saveGray filename im = do
    handle <- openFile (filename++".8u") WriteMode
    let (ps,c) = rowPtrs im
        f p = hPutBuf handle p c
        Size h w = roiSize (roi im)
    withImage im $ do
        mapM_ f ps
        hClose handle
    _ <- system $ "convert -size "++show w++"x"++show h++" -depth 8 gray:"
             ++(filename++".8u ")++filename
    _ <- system $ "rm "++(filename++".8u")
    return ()

----------------------------------------------------------------------

-- | Load an image using imagemagick's convert.
loadGray :: FilePath -> IO ImageGray
loadGray filename = do
    Size h w <- getSize filename
    let fname = fixSpaces filename
    mh <- getOption "--maxHeight" h
    let (h',w') = fixSizes mh h w
    _ <- system $ "convert -resize "++show w' ++"x"++show h'++"! "++fname++" -depth 8 gray:"
             ++(fname++".8u ")
    handle <- openFile (filename++".8u") ReadMode
    im <- newImage undefined (Size h' w')
    let (ps,c) = rowPtrs im
        f p = hGetBuf handle p c
    withImage im $ do
        mapM_ f ps
        hClose handle
    _ <- system $ "rm "++(fname++".8u")
    return im

-- | Load an image using imagemagick's convert.
loadRGB :: FilePath -> IO ImageRGB

loadRGB filename | ".ppm" `isSuffixOf` (map toLower filename) = loadRawPPM filename

loadRGB filename = do
    Size h w <- getSize filename
    let fname = fixSpaces filename
    mh <- getOption "--maxHeight" h
    let (h',w') = fixSizes mh h w
    -- print (h,w)
    -- print (h',w')
    --print filename
    _ <- system $ "convert -resize "++show w' ++"x"++show h'++"! "++fname++" -depth 8 rgb:"
             ++(fname++".rgb")
    handle <- openFile (filename++".rgb") ReadMode
    im <- newImage undefined (Size h' w')
    let (ps,c) = rowPtrs im
        f p = hGetBuf handle p (c*3)
    withImage im $ do
        mapM_ f ps
        hClose handle
    _ <- system $ "rm "++(fname++".rgb")
    return im

fixSizes :: Int -> Int -> Int -> (Int, Int)
fixSizes mh h w = (mkEven *** mkEven) $ if h > mh then (mh, (mh*w) `div` h) else (h,w)
  where
    mkEven n = ((n+1) `div` 2) * 2

fixSpaces :: String -> String
fixSpaces = concatMap f
  where
    f ' ' = "\\ "
    f x = [x]

getSize :: FilePath -> IO Size
getSize imagfile = do
    s <- readProcess "identify" [imagfile] ""
    return (g . words . map f . he . words $ s)
  where
    f 'x' = ' '
    f a = a
    g [w,h] = Size (read h) (read w)
    g _ = error "getSize"
    he = head . tail . dropWhile (not . (`elem` ["PNG","JPEG","BMP"])) -- FIXME check error

--------------------------------------------------------------------------------

loadRawPPM :: FilePath -> IO ImageRGB
loadRawPPM filename = do
    handle <- openFile filename ReadMode
    sh <- header 4 handle
    -- print sh
    let ["P6",sw,sr,"255"] = sh
    let sz@(Size r c) = Size (read sr) (read sw)
    im <- newImage undefined sz
    withImage im $ do
        if c `mod` 32 /= 0
          then do
            let (ps,c') = rowPtrs im
                f p = hGetBuf handle p (c'*3)
            mapM_ f ps
          else do
            _ <- hGetBuf handle (starting im) (r*c*3)
            return ()
            
        hClose handle
    return im
  where
    header n h = do
    ws <- words . takeWhile (/='#') <$> hGetLine h
    let nw = length ws
    if nw < n
      then do more <- header (n-nw) h
              return (ws++more)
      else return ws


--------------------------------------------------------------------------------

saveRawPPM :: FilePath -> ImageRGB -> IO ()
saveRawPPM filename Image{..} = do
    BS.writeFile filename (BS.append header pixels)
  where
    Size h w = size
    header = BSC.pack . unlines $
      [ "P6"
      , "# created by hVision"
      , intercalate " " . map show $ [w, h]
      , "255"
      ]
    pixels | w `mod` 32 == 0 = bytes


savePPM :: Maybe FilePath -> ImageRGB -> IO FilePath
savePPM (Just filename) x = saveRawPPM filename x >> return filename

savePPM Nothing x = do
    timename <- formattedTime
    let filename = timename++".ppm"
    savePPM (Just filename) x

