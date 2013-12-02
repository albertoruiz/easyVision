{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Image.Convert
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Convert (
    saveImage,
    loadRGB,
    convert,
    img2mat, mat2img
) where

import Image.Core
import Foreign.Ptr ( plusPtr )
import Foreign.Marshal ( copyBytes )
import Control.Monad ( when )
import Control.Applicative ( (<$>) )
import System.IO
import System.Exit ( ExitCode(ExitSuccess) )
import System.Process ( system )
import Data.List ( isSuffixOf, intercalate )
import Data.List.Split ( splitOn )
import Data.Packed.Development
import Numeric.LinearAlgebra ( Matrix, rows, cols )
import Data.Char ( toLower )
import qualified Data.ByteString as BS ( writeFile, append )
import qualified Data.ByteString.Char8 as BSC ( pack )
import Util.Misc ( formattedTime )
import System.IO.Unsafe(unsafePerformIO)

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
        g t _ p = do
            sequence_ $ zipWith (f p) ps [0..fromIntegral t-1]
            return 0
    m <- createMatrix RowMajor r c
    withImage im $ do
        app1 g mat (cmat m) "img2mat"
    return m

----------------------------------------------------------------------

-- | Load an image using imagemagick's convert.
loadRGB :: FilePath -> IO ImageRGB
loadRGB filename
    | ".ppm" `isSuffixOf` (map toLower filename) = loadRawPPM filename
    | otherwise = do
          name <- convert "ppm" filename
          img <- loadRawPPM name
          system' $ "rm "++name
          return img

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
saveRawPPM filename im@Image{..} = do
    if w `mod` 32 == 0 && False
      then
        BS.writeFile filename (BS.append header bytes)
      else do
        BS.writeFile filename header
        appendData
  where
    Size h w = size
    header = BSC.pack . unlines $
      [ "P6"
      , "# created by hVision"
      , intercalate " " . map show $ [w, h]
      , "255"
      ]
    appendData = do
        handle <- openFile filename AppendMode
        let (ps,c) = rowPtrs im
            f p = hPutBuf handle p (c*3)
        withImage im $ mapM_ f ps
        hClose handle


savePPM :: Maybe FilePath -> ImageRGB -> IO FilePath
savePPM (Just filename) x = saveRawPPM filename x >> return filename

savePPM Nothing x = do
    timename <- formattedTime
    let filename = redu timename++".ppm"
    savePPM (Just filename) x
  where
    redu = map g . filter (/='-')
    g 'T' = '-'
    g z   = z


nameext :: FilePath -> (String,String)
nameext p | null things = (p,"")
          | otherwise   = (name,ext)
  where
    things = splitOn "." p
    name = intercalate "." (init things)
    ext  = last things

-- nameext ".b"

saveImage :: FilePath -> ImageRGB -> IO ()
saveImage (nameext -> ("",""))    x = savePPM Nothing x >> return ()
saveImage (nameext -> ("","ppm")) x = savePPM Nothing x >> return ()
saveImage (nameext -> ("ppm","")) x = savePPM Nothing x >> return ()
saveImage (nameext -> ("",ext))   x = savePPM Nothing x >>= convertTo ext
saveImage (nameext -> (name,""))  x
   | name `elem` ["png","jpg","bmp"] = savePPM Nothing x >>= convertTo name
   | otherwise = savePPM (Just (name++".ppm")) x >> return ()
saveImage n@(nameext -> (_,"ppm")) x = savePPM (Just n) x >> return ()
saveImage (nameext -> (name,ext)) x = savePPM (Just (name++".ppm")) x >>= convertTo ext


convertTo :: String -> FilePath -> IO ()
convertTo newext filename@(nameext -> (name,_)) = system' cmd
  where
    cmd = unwords ["convert",filename,newname,"&& rm", filename]
    newname = name++"."++newext
  
convert :: String -> FilePath -> IO FilePath
convert newext filename@(nameext -> (name,_)) = system' cmd >> return newname
  where
    cmd = unwords ["convert",filename,newname]
    newname = name++"."++newext

system' :: String -> IO ()
system' cmd = do
    ok <- system cmd
    when (ok /= ExitSuccess) $ error $ "system " ++ cmd
    return ()

