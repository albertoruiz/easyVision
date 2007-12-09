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
    catalogBrowser
)where

import Graphics.UI.GLUT hiding (Size)
import EasyVision.GUI
import ImagProc
import Data.List(transpose)
import Control.Monad(when)

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

-- |
catalogBrowser n catalog name sz =
    evWindow (n-1,catalog) "catalog" sz (Just disp) (mouse $ kbdQuit)
  where
    disp st = do
        (k,catalog) <- get st
        let (img,label) = catalog!!k
        drawImage img
        windowTitle $= "#"++show (k+1)++ ": "++label
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (k,catalog) <- get st
        st $= (min (k+1) (length catalog -1), catalog)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        (k,catalog) <- get st
        st $= (max (k-1) 0, catalog)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d
