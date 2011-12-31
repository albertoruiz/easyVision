-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Save
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


A camera combinator for saving frames.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Save (
  saveWin
)where


import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when)
import EasyVision.GUI
import ImagProc.Ipp.Core
import ImagProc.Camera
import ImagProc.Util(autoSaver)
import Util.Options


saveWin :: (b -> ImageYUV) -> IO b -> IO (IO b)
saveWin f cam = do
    save <- autoSaver
    ok <- hasValue "--save"
    wait <- getFlag "--wait"
    let title = "Click to start/stop recording"
    w <- evWindow (not wait && ok) title (mpSize 10) Nothing (mouse kbdQuit)
    return $ do
        orig <- cam
        rec <- getW w
        inWin w $ do
            let x = f orig
            drawImage x
            when rec $ do
                setColor 1 0 0
                text2D 25 60 "Recording..."
                save x
        return orig
  where        
    mouse _ w (MouseButton LeftButton) Down _ _ = updateW w not
    mouse def _ a b c d = def a b c d


