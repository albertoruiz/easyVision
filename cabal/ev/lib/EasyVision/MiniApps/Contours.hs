{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss.Contours
Copyright   :  (c) Alberto Ruiz 2010-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Contours (
    ContourParam(..), winContourParam, argContourParam, defContourParam,
    ContourInfo(..), smartContours, contourMonitor, wcontours,
    renderPolyline,
    shapeCatalog
) where

import EasyVision.MiniApps.Combinators
import EasyVision.GUI
import ImagProc
import ImagProc.Util
import ImagProc.Camera
import Graphics.UI.GLUT hiding (Size,Point)
import Features hiding(area)
import EasyVision.MiniApps.Browser(examplesBrowser)
import Util.Misc(diagl,impossible)
import Classifier(Sample)
import Data.IORef(readIORef,writeIORef)
import Data.Colour.Names(orange)
import Control.Monad(when)
import Data.Maybe(isJust)
import Control.Arrow((***))
import Data.List(minimumBy)
import Numeric.LinearAlgebra((<>))
import Vision(desp,scaling)


$(autoParam "ContourParam" "contour-" [
    ("thres",  "Int",    intParam 128 1 255),
    ("area",   "Int",    percent 10),
    ("fracpix","Double", realParam (1.5) 0 10),
    ("mode",   "String", stringParam "black" ["white", "black", "both"]),
    ("smooth", "Int",    intParam 1 0 10),
    ("thresDelta", "Int",    intParam 16 0 255),
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

----------------------------------------------------------------------

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
        mapM_ renderPolyline (contSel cs)

----------------------------------------------------------------------

-- | load/add/save/store shapes
shapeCatalog :: (Drawable t, Image t)
             => (Polyline -> Polyline) -- ^ preprocessing
             -> IO (Sample Polyline)   -- ^ initial set
             -> (Sample Polyline -> [a]) -- ^ feature extraction
             -> IO (t, [Polyline])       -- ^ input camera with polylines
             -> IO (IO (t, [Polyline], [a])) -- ^ input and features of current set
shapeCatalog prepro gprot feat' cam = do
    oraw <- gprot
    let feat = feat' . map (prepro *** id)
        prototypes = feat oraw
    (cam',ctrl) <- withPause cam
    w <- evWindow (oraw,prototypes,Nothing,False) "Contour Selection" (mpSize 10) Nothing (marker (kbdcam ctrl))
    s <- shapesBrowser "Shape" (Size 240 240) oraw
    return $ do
        (raw',prots',click,save) <- getW w
        (im,cs) <- cam'
        let sz = size im -- required to avoid space leak !?
            (raw,prots) = case click of
                Nothing -> (raw',prots')
                Just pix  -> let [pt] = pixelsToPoints sz [pix]
                                 newc = (prepro $ closestTo pt cs, "?" ++ show (length prots' + 1))
                           in (newc:raw', feat [newc] ++ prots')
        when (isJust click) $ putW s (0, raw) >> postRedisplay (Just (evW s))
        inWin w $ do
            drawImage' im
            pointCoordinates sz
            text2D 0.9 0.6 $ show (length cs)
            setColor' orange
            mapM_ renderPolyline cs
            
        when save $ writeFile "shapes.txt" (show raw)
        putW w (raw,prots,Nothing,False)
        return (im,cs,prots)
  where
    marker _ st (MouseButton LeftButton) Down _ (Position x y) = do
        (r,p,_,_) <- readIORef st
        let clicked = Pixel (fromIntegral y) (fromIntegral x)
        writeIORef st (r,p, Just clicked, False)

    marker _ st (Char 'S') Down _ _ = do
        (r,p,_,_) <- readIORef st
        writeIORef st (r,p, Nothing, True)

    marker def _ a b c d = def a b c d

    closestTo pt = minimumBy (compare `on` (d pt))
      where
        d p c = distPoints p (cen c)
        cen (Closed c) = Point cx cy where (cx,cy,_,_,_) = momentsContour c
        cen _ = impossible "center of open polyline in shapeCatalog"

    shapesBrowser name sz = examplesBrowser name sz f
      where
        f = renderPolyline . transPol (diagl[-0.4,0.4,1]) . normalShape

----------------------------------------------------------------------

renderPolyline :: Polyline -> IO ()
renderPolyline c@(Closed _) = renderPrimitive LineLoop (vertex c)
renderPolyline c@(Open _) = renderPrimitive LineStrip (vertex c)

----------------------------------------------------------------------

