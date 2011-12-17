-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Browser
Copyright   :  (c) Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Show in a window each element of a list using the mouse wheel.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Browser (
    examplesBrowser,
    editBrowser,
    imagesBrowser
)where

import EasyVision.GUI
import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import Classifier(Sample)
import ImagProc
import Data.Colour.Names as Col
import Control.Monad(when)
import Util.Misc(replaceAt)

----------------------------------------------------------------------

-- | a window to display a list of labeled objects
examplesBrowser :: String -> Size -> (t -> IO ()) -> Sample t -> IO (EVWindow (Int, Sample t))
examplesBrowser name sz f es =
    evWindow (0,es) name sz (Just disp) (mouseGen acts kbdQuit)
  where
    disp st = do
        pointCoordinates sz
        (k,exs) <- get st
        when (not $ null exs) $ do 
            let (x,label) = exs!!k
            f x
            windowTitle $= name++" #"++show (k+1)++ ": "++label
    acts = [((MouseButton WheelUp,   Down, modif), \_ (k,exs) -> (min (k+1) (length exs - 1), exs))
           ,((SpecialKey  KeyUp,     Down, modif), \_ (k,exs) -> (min (k+1) (length exs - 1), exs))
           ,((MouseButton WheelDown, Down, modif), \_ (k,exs) -> (max (k-1) 0, exs))
           ,((SpecialKey  KeyDown,   Down, modif), \_ (k,exs) -> (max (k-1) 0, exs))]


-- | a window to display a list of labeled objects with some edit capability
editBrowser :: Show b
            => String  -- ^ window name
            -> Size    -- ^ windows size
            -> (t -> IO a)  -- ^ display function
            -> [((Key, KeyState, Modifiers), Int -> Int -> t -> t)] -- ^ custom object modification commands
            -> ((t, String) -> b) -- ^ projection function for saving to a file
            -> [(t, String)] -- ^ labeled objects
            -> IO (EVWindow (Bool, Int, [(t, [Char])]))
editBrowser name sz f kuser svf es =
    evWindow (False,0,es) name sz (Just disp) (mouseGen (acts++ map g kuser) kbdQuit)
  where
    disp st = do
        pointCoordinates sz
        (sv,k,exs) <- get st
        when sv $ do
            writeFile "saved.txt" (show $ map svf exs)
            st $= (False,k,exs)
        when (not $ null exs) $ do 
            let (x,label) = exs!!k
            f x
            windowTitle $= name++" #"++show (k+1)++ ": "++label
    acts = [((Char 's',   Down, modif), \_ (_,k,exs) -> (True, k, exs))
           ,((MouseButton WheelUp,   Down, modif), \_ (_,k,exs) -> (False, min (k+1) (length exs - 1), exs))
           ,((SpecialKey  KeyUp,     Down, modif), \_ (_,k,exs) -> (False, min (k+1) (length exs - 1), exs))
           ,((MouseButton WheelDown, Down, modif), \_ (_,k,exs) -> (False, max (k-1) 0, exs))
           ,((SpecialKey  KeyDown,   Down, modif), \_ (_,k,exs) -> (False, max (k-1) 0, exs))]
    g (k, fun) = (k, \(Position x y) (_, k,exs) -> (False, k, replaceAt [k] [h fun x y (exs!!k)] exs)) 
    h user x y (t,s) = (user (fromIntegral x::Int) (fromIntegral y:: Int) t, s)



-- | a window to display labeled images
imagesBrowser :: (Image t, Drawable t) => String -> Size -> Sample t -> IO (EVWindow (Int, Sample t))
imagesBrowser name sz = examplesBrowser name sz f
  where
    f img = do
        drawImage' img
        pixelCoordinates (size img)
        setColor' red
        text2D 20 20 (shSize img)
    shSize x = show w ++ "x" ++ show h
      where Size h w = size x

