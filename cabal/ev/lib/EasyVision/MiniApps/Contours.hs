{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss.Contours
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Contours (
    ContourParam(..), winContourParam, argContourParam, defContourParam,
    ContourInfo(..), smartContours, contourMonitor, wcontours
) where

import EasyVision.MiniApps.Combinators
import EasyVision.GUI
import ImagProc
import ImagProc.Util
import ImagProc.Camera
import Graphics.UI.GLUT hiding (Size)
import Features
-- import Control.Monad((>=>))
-- import Control.Applicative
-- import Control.Arrow((&&&),(***))


$(autoParam "ContourParam" "contour-" [
    ("thres",  "Int",    intParam 128 1 255),
    ("area",   "Int",    percent 10),
    ("fracpix","Double", realParam (1.5) 0 10),
    ("mode",   "String", stringParam "white" ["white", "black", "both"]),
    ("smooth", "Int",    intParam 1 0 10),
    ("thresDelta", "Int",    intParam 32 0 255),
    ("thresRange", "Int",    intParam 0 0 10)] )


defContourParam :: ContourParam
argContourParam :: IO ContourParam
winContourParam :: IO (IO ContourParam)

wcontours :: (x -> ImageGray) -> IO x ->  IO (IO (x, ContourInfo))
wcontours g = smartContours g .@. winContourParam

smartContours :: (x -> ImageGray) -> ContourParam -> x -> ContourInfo
smartContours g ContourParam{..} x = r
    where pre = (smooth `times` median Mask3x3) . g
          z = pre x
          rawg b y = concatMap (\th -> map fst3 $ contours 100 pixarea (fromIntegral th) b y)
                     [thres-thresDelta*thresRange,
                      thres-thresDelta*(thresRange-1) .. 
                      thres+thresDelta*thresRange]
               where (Size h w) = size y
                     pixarea = h*w*area`div`10000
          cw = post $ rawg True z
          cb = post $ rawg False z
          cwb = cw ++ cb
          cs = case mode of
                    "white" -> cw
                    "black" -> cb
                    "both"  -> cwb
                    _ -> error "smartContours unknown mode"
          redu = douglasPeuckerClosed fracpix
          clo  = Closed . pixelsToPoints (size z)
          times n f = (!!n) . iterate f
          fst3 (a,_,_) = a
          post = map (clo . redu)
          r = ContourInfo { contWhite = cw
                          , contBlack = cb
                          , contBoth  = cwb
                          , contSel   = cs }

data ContourInfo = ContourInfo {
    contWhite :: [Polyline],
    contBlack :: [Polyline],
    contBoth  :: [Polyline],
    contSel   :: [Polyline]
    }

-----------------------------------------------------

contourMonitor :: (Image t, Drawable t) 
               => String
               -> IO ()
               -> IO (t, ContourInfo)
               -> IO (IO (t, ContourInfo))
contourMonitor winname f = monitor winname (mpSize 20) sh where
    sh (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        f
        mapM_ shcont (contSel cs)
    shcont (Closed c) = do
        renderPrimitive LineLoop $ mapM_ vertex c
    shcont (Open c) = do
        renderPrimitive LineStrip $ mapM_ vertex c
