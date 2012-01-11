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
    ContourInfo(..), 
    ContourParam(..), winContourParam, argContourParam, defContourParam,
    smartContours, wcontours,
    contourMonitor, 
    shapeCatalog, shapeCatalog',
    PolygonParam(..), winPolygonParam, argPolygonParam, defPolygonParam,
    polygonalize,
    drawContourLabeled
) where

import EasyVision.MiniApps.Combinators
import EasyVision.GUI
import ImagProc
import ImagProc.Camera
import Graphics.UI.GLUT hiding (Size,Point)
import Contours hiding(area)
import Features.Polyline
import EasyVision.MiniApps.Browser(examplesBrowser)
import Util.Misc(diagl,impossible,degree)
import Classifier(Sample)
import Data.IORef(readIORef,writeIORef)
import Data.Colour.Names(orange)
import Control.Monad(when)
import Data.Maybe(isJust)
import Control.Arrow((***))
import Data.List(minimumBy)
import Data.Colour.Names as Col

data ContourInfo = ContourInfo {
    contWhite :: [Polyline],
    contBlack :: [Polyline],
    contBoth  :: [Polyline],
    contSel   :: [Polyline]
    }

autoParam "ContourParam" "contour-" [
    ("athres",  "Int",    intParam 1 (-20) 10),
    ("thres",  "Int",    intParam 128 1 255),
    ("area",   "Int",    percent 10),
    ("fracpix","Double", realParam (1.5) 0 10),
    ("mode",   "String", stringParam "black" ["white", "black", "both"]),
    ("auto",   "Int",    intParam 0 0 1),
    ("smooth", "Int",    intParam 1 0 10),
    ("asmooth", "Int",    intParam 0 0 10),
    ("thresDelta", "Int",    intParam 16 0 255),
    ("thresRange", "Int",    intParam 0 0 10),
    ("radius", "Int",    intParam 10 0 30)]


defContourParam :: ContourParam
argContourParam :: IO ContourParam
winContourParam :: IO (IO ContourParam)

-- | find closed contours
wcontours :: (x -> ImageGray) -> IO x ->  IO (IO (x, ContourInfo))
wcontours g = smartContours g .@. winContourParam

smartContours :: (x -> ImageGray) -> ContourParam -> x -> ContourInfo
smartContours g ContourParam{..} x = r
    where pre 0 = (smooth `times` median Mask3x3) . g
          pre 1 = (asmooth `times` median Mask3x3) . autoThres radius . g
          z = pre auto x
          pixarea = h*w*area`div`10000 where Size h w = size z
              
          rawg 1 True y = map fst3 $ contours 100 pixarea (128-fromIntegral athres) True y
          rawg 1 False y = map fst3 $ contours 100 pixarea (128+fromIntegral athres) False y

          rawg 0 b y = concatMap (\th -> map fst3 $ contours 100 pixarea (fromIntegral th) b y)
                     [thres-thresDelta*thresRange,
                      thres-thresDelta*(thresRange-1) .. 
                      thres+thresDelta*thresRange]

          cw = post $ rawg auto True z
          cb = post $ rawg auto False z
          cwb = cw ++ cb
          cs = case mode of
                    "white" -> cw
                    "black" -> cb
                    "both"  -> cwb
                    _ -> error "smartContours unknown mode"
          redu | fracpix > 0.01 = douglasPeuckerClosed fracpix
               | otherwise   = id
          clo  = Closed . pixelsToPoints (size z)
          times n f = (!!n) . iterate f
          fst3 (a,_,_) = a
          post = map (clo . redu)
          r = ContourInfo { contWhite = cw
                          , contBlack = cb
                          , contBoth  = cwb
                          , contSel   = cs }


----------------------------------------------------------------------

autoThres r x = sh d
  where
    f = float x
    s = filterBox r r f
    d = f |-| s
    sh = scale32f8u (-1) 1

----------------------------------------------------------------------

-- | to be optionally used after wcontours
contourMonitor :: String
               -> (x -> ImageGray)  -- ^ selector
               -> IO ()             -- ^ graphic primitives previous to renderPolyline
               -> (x -> [Polyline]) -- ^ selector
               -> IO x
               -> IO (IO x)
contourMonitor winname selImg f selCont = monitor winname (mpSize 10) sh where
    sh x = do
        let im = selImg x
            cs = selCont x
        drawImage' im
        pointCoordinates (size im)
        f
        mapM_ renderPolyline cs

----------------------------------------------------------------------

-- | load/add/save/store shapes
shapeCatalog
    :: (Drawable t, Image t)
    => (x -> t)                 -- ^ selector of image from the input (e.g. fst when using wcont)
    -> (x -> [Polyline])        -- ^ selector of polylines from the input (e.g snd or selCont . snd)
    -> (Polyline -> Polyline)   -- ^ preprocessing
    -> IO (Sample Polyline)     -- ^ initial set
    -> (Sample Polyline -> [a]) -- ^ feature extraction
    -> IO x                     -- ^ input camera with polylines
    -> IO (IO (x, [a]))         -- ^ input and features of current set
shapeCatalog = shapeCatalog' False

shapeCatalog' hide fimg fpoly prepro gprot feat' cam = do
    oraw <- gprot
    let feat = feat' . map (prepro *** id)
        prototypes = feat oraw
    (cam',ctrl) <- withPause cam
    w <- evWindow (oraw,prototypes,Nothing,False) "Contour Selection" (mpSize 10) Nothing (marker (kbdcam ctrl))
    when hide (windowStatus $= Hidden)
    s <- shapesBrowser "Shape" (Size 240 240) oraw
    when hide (windowStatus $= Hidden)
    return $ do
        (raw',prots',click,save) <- getW w
        x <- cam'
        let im = fimg x
            cs = fpoly x
        let sz = size im -- required to avoid space leak !?
            (raw,prots) = case click of
                Nothing -> (raw',prots')
                Just pix  -> let [pt] = pixelsToPoints sz [pix]
                                 newc = (prepro $ closestTo pt cs, "?" ++ show (length prots' + 1))
                           in (newc:raw', feat [newc] ++ prots')
        when (isJust click && not (null cs)) $ putW s (0, raw) >> postRedisplay (Just (evW s))
        inWin w $ do
            drawImage' im
            pointCoordinates sz
            text2D 0.9 0.6 $ show (length cs)
            setColor' orange
            mapM_ renderPolyline cs
            
        when save $ writeFile "shapes.txt" (show raw)
        putW w (raw,prots,Nothing,False)
        return (x,prots)
  where
    marker _ st (MouseButton LeftButton) Down _ (Position x y) = do
        (r,p,_,_) <- getW st
        let clicked = Pixel (fromIntegral y) (fromIntegral x)
        putW st (r,p, Just clicked, False)

    marker _ st (Char 'S') Down _ _ = do
        (r,p,_,_) <- getW st
        putW st (r,p, Nothing, True)

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

autoParam "PolygonParam" "polygon-"
  [( "eps","Double" ,realParam 10 0 50),
   ( "sides","Int" ,intParam 4 3 10),
   ( "tol","Double" ,realParam 10 0 45)]

polygonalize PolygonParam {..} = id *** selectPolygons (eps/1000) sides . map (cleanPol (cos $ tol*degree))

----------------------------------------------------------------------

drawContourLabeled (Closed c) = do
    lineWidth $= 1
    setColor' green
    renderPrimitive LineLoop (vertex (Closed c))
    setColor' red
    pointSize $= 5
    renderPrimitive Points (vertex $ head c)
    pointSize $= 3
    setColor' orange
    renderPrimitive Points (vertex $ Closed $ tail c)
    setColor' blue
    sequence_ (zipWith textAt c (map show [0..]))    

