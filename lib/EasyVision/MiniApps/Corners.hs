{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss.Corners
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Corners (
    cornerDetector, cornerMonitor,
    cornerDetectorP, getCornerParam, getCornerDetector
) where

import EasyVision.Combinators
import EasyVision.Util
import EasyVision.Draw
import EasyVision.GUI
import EasyVision.Parameters
import EasyVision.Concurrent
import ImagProc
import Graphics.UI.GLUT
import Control.Monad((>=>))
import Control.Applicative
import Control.Arrow((&&&),(***))


f .***. g = uncurry zip . (f *** g) . unzip
f .&&&. g =  (f .***. g) . map (id &&& id)

f .| g = f . pipeline g

addFunc genpar cam = do
    par <- genpar
    return $ (,) <$> cam <*> par

-----------------------------------------------------------

cornerMonitor winname = monitor winname (mpSize 20) sh

getCornerDetector = do
    pipe <- getFlag "-p"
    param <- getCornerParam
    let cd= if pipe
            then cornerDetectorP param
            else cornerDetector
    return cd

------------------------------------------------------------

cornerDetector = addFunc userParam
             >~> fst &&& uncurry (flip corners)
--           >=> cornerMonitor "corners"

cornerDetectorP par = virtualCamera (id .&&&. cornersP par)
--           >=> cornerMonitor "corners"

-- to do: inject parameters in the pipeline

-------------------------------------------------------------

corners Param{mode = "normal", ..} =
                    getPoints32f 300
                  . localMax rad
                  . threshold thres
                  . gaussS' smask sigma2
                  . sqrt32f
                  . abs32f
                  . hessian
                  . gradients
                  . gaussS' smask sigma1

corners Param{mode = "fast", ..} =
                    getPoints32f 300
                  . localMax rad
                  . threshold (thres^2)
                  . ((-1).*)
                  . hessian
                  . gradients
                  . gaussS' smask sigma1

cornersP Param{mode = "normal", ..} =
                     pipeline (getPoints32f 300)
                  .| localMax rad
                  .| threshold thres
                  .| gaussS' smask sigma2
                  .| sqrt32f
                  .| abs32f
                  .| (hessian . gradients)
                  .| gaussS' smask sigma1

cornersP Param{mode = "fast", ..} =
                    pipeline (getPoints32f 300 . localMax rad . threshold (thres^2) . ((-1).*))
                  .| (hessian . gradients)
                  .| gaussS' smask sigma1


threshold r im = thresholdVal32f (mx*r) 0 IppCmpLess im
    where (_,mx) = ImagProc.minmax im

sh (im, pts) = do
    drawImage' im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts

-----------------------------------------------------------------

data Param = Param { sigma1 :: Float
                   , sigma2 :: Float
                   , smask  :: Float
                   , rad    :: Int
                   , thres  :: Float
                   , mode   :: String }

userParam = do
    Param{..} <- getCornerParam

    o <- createParameters' "Corners"
        [("sigma1",floatParam  sigma1 0 20),
         ("sigma2",floatParam  sigma2 0 20),
         ("smask", floatParam  smask  0 5),
         ("rad"  , intParam    rad    1 25),
         ("thres", floatParam  thres  0 1),
         ("mode",  stringParam mode   ["normal","fast"])]

    return $ Param <$> getParam o "sigma1"
                   <*> getParam o "sigma2"
                   <*> getParam o "smask"
                   <*> getParam o "rad"
                   <*> getParam o "thres"
                   <*> getParam o "mode"

getCornerParam = Param <$> getOption "--sigma1" 2
                       <*> getOption "--sigma2" 3
                       <*> getOption "--smask"  2
                       <*> getOption "--rad"    1
                       <*> getOption "--thres"  0.4
                       <*> optionString "--mode" "normal"
