-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Static
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


Detector of static frames.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Static (
  detectMotion,
  detectStatic,
  temporalEnvironment,
)where

import Graphics.UI.GLUT hiding (Size,Point)
import Control.Monad(when,(>=>),forever)
import EasyVision.GUI
import ImagProc.Ipp.Core
import EasyVision.MiniApps.Misc(addSmall)
import EasyVision.MiniApps.Combinators(monitor)
import Data.List(tails)
import ImagProc.Camera(mpSize)
import ImagProc


-- | Motion detector with a desired condition on the absolute pixel difference in the roi of two consecutive images (computed with help of 'addSmall').
detectMotion :: (Double -> Bool) -> IO (ImageYUV, ImageGray) -> IO (IO (ImageYUV, ImageGray))
detectMotion cond = virtualCamera (detectMov' cond)

detectMov' cond ((a,f):(b,g):t) =
    if cond (absdif f g)
        then (a,f) : detectMov' cond ((b,g):t)
        else detectMov' cond ((b,g):t)

absdif a b = sum8u (absDiff8u a b)


-- | Camera combinator which creates a list of the past and future values of a property in each frame (and applies a given function to it).
temporalEnvironment :: ([p] -> x) -- ^ function to apply to the temporal enviroment (you can of course use id)
                    -> Int        -- ^ number of frames in the past
                    -> Int        -- ^ number of frames in the future
                    -> IO (a,p)   -- ^ source camera
                    -> IO (IO (a, x)) -- ^ result
temporalEnvironment g past future = virtualCamera f where
    f = map (adjust . unzip) . slidingGroup time
    time = past + future + 1
    adjust (cams, env)  = (cams!!past, g env)
    slidingGroup n = map (take n) . tails

----------------------------------------------------------
-- Detector of static frames

addDiff = virtualCamera auxDif
    where auxDif ((a,f):(b,g):t) = (a, nrm (size f) $ absdif f g) : auxDif ((b,g):t)
          nrm (Size r w) = (/n) where n = fromIntegral (r*w*255)

data StaticDetectorState = SDSGetIt
                         | SDSWaitUp
                         | SDSWaitDown
                         deriving (Eq,Show)

auxStatic th thf SDSGetIt x = SDSWaitUp

auxStatic th thf SDSWaitUp x | last x > thf*th = SDSWaitDown
                             | otherwise       = SDSWaitUp

auxStatic th thf SDSWaitDown x | maximum x < th = SDSGetIt
                               | otherwise      = SDSWaitDown

addSDS th thf = virtualCamera (f SDSWaitUp)
    where f st ((c,p):rest) = (c,(p,st')) : f st' rest
               where st' = auxStatic th thf st p

getIt xs = fst (head xs) : [c | (c,(_,st)) <- xs, st == SDSGetIt ]

monitorStatic th thf g (imag,(env,st)) = do
    let x = g imag
    drawImage x
    setColor 1 0 0
    text2D 30 30 (show st)
    pointCoordinates (size x)
    lineWidth $= 2
    renderSignal (map (10*) env)
    setColor 0.5 0 0
    lineWidth $= 1
    renderAxes
    renderPrimitive Lines $ mapM_ vertex [Point (-0.1) (10*th), Point 0.1 (10*th)]
    renderPrimitive Lines $ mapM_ vertex [Point (-0.1) (thf*10*th), Point 0.1 (thf*10*th)]
    mainLoopEvent

-- | Detector of static frames.
detectStatic :: (Drawable b, Image b)
             => Double -- ^ threshold (.e.g., 0.01)
             -> Double -- ^ high threshold factor (.e.g., 5)
             -> Int    -- ^ number of frames which must be \"quiet\" (e.g. 5)
             -> (a -> ImageGray) -- ^ selector
             -> (a -> b)         -- ^ monitor
             -> IO a -- ^ source camera
             -> IO (IO a) -- ^ virtual camera
detectStatic th thf nframes f g =
    addSmall (mpSize 3) f
    >=> addDiff
    >=> temporalEnvironment id nframes 0
    >=> addSDS th thf
    >=> monitor "temp env" (mpSize 10) (monitorStatic th thf g)
    >=> virtualCamera getIt

