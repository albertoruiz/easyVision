-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Useful windows with associated behaviour.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps (
    readCatalog,
    catalogBrowser,
    hsvPalette
)where

import Graphics.UI.GLUT as GL hiding (Size)
import EasyVision.GUI
import ImagProc
import Data.List(transpose)
import Control.Monad(when)
import ImagProc.Ipp.Core
import Foreign.C.Types(CUChar)
import Foreign

-- | reads a labeled video
readCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
readCatalog video sz classesfile mbn prepro = do
    cam <- mplayer (video++" -benchmark") sz
    rawclasses <- readFile classesfile
    let classfilelines = lines rawclasses
        effectivelines = case mbn of
            Nothing -> classfilelines
            Just n  -> take n classfilelines
    let n = length effectivelines
        words' s = let (n:ws) = words s in [n , unwords ws]
    let [frames, classes] = transpose $ map words' effectivelines
    when (map read frames /= [1..n]) $ error ("inconsistent file "++ classesfile)
    imgs <- sequence (replicate n cam)
    return (zip (map prepro imgs) classes)

-- | to do (ImageYUV???)
catalogBrowser :: Int -> [(ImageYUV, String)] -> String -> Size -> IO (EVWindow (Int, [(ImageYUV, String)]))
catalogBrowser n catalog name sz =
    evWindow (n-1,catalog) name sz (Just disp) (mouse $ kbdQuit)
  where
    disp st = do
        (k,catalog) <- get st
        let (img,label) = catalog!!k
        drawImage img
        windowTitle $= name++" #"++show (k+1)++ ": "++label
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (k,catalog) <- get st
        st $= (min (k+1) (length catalog -1), catalog)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        (k,catalog) <- get st
        st $= (max (k-1) 0, catalog)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

-------------------------------------------------------------------------------------

hsvPalette :: IO (EVWindow (CUChar, CUChar, CUChar))
hsvPalette = evWindow (128,128,255) "HSV" (Size 256 256) (Just disp) (mouse kbdQuit)
  where
    disp st = do
        (r',c',k) <- get st
        drawImage =<< palette k
        pixelCoordinates (Size 256 256)
        setColor 0 0 0
        let r = fromIntegral r'
            c = fromIntegral c'
        renderPrimitive LineLoop $ mapM_ vertex
            [Pixel (r-2) (c-2), Pixel (r-2) (c+2), Pixel (r+2) (c+2), Pixel (r+2) (c-2)]
        text2D 20 20 (show (c,r,k))
    mouse _ st (MouseButton WheelUp) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k + if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k- if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton LeftButton) Down m (Position x y) = do
        (_,_,k) <- get st
        st $= (fromIntegral y, fromIntegral x,k)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d


palette k = hsvToRGB $ putChannels (fromLists ramp ,
                                    fromLists (transpose ramp),
                                    constImage k (Size 256 256))
    where
        ramp = replicate 256 [0..255]

        fromLists ls = unsafePerformIO $ do
            r <- image (Size 256 256)
            setData8u r ls
            return r
