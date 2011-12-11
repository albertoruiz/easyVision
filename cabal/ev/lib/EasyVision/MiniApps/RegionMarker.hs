{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.RegionMarker
Copyright   :  (c) Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


A combinator for selecting a region (as a polyline) in the image.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.RegionMarker (
  regionMarker,
  rectifyRegion, AspectRatioParam(..), winAspectRatioParam, argAspectRatioParam, defAspectRatioParam
)where


import Graphics.UI.GLUT hiding (Size,Point)
import Contours.Polyline
import EasyVision.GUI
import ImagProc.Ipp.Core(Size(..),Image,Point(..),Pixel(..),distPoints,pixelsToPoints,Polyline(..))
import ImagProc(GImg(..),warp)
import ImagProc.Camera(mpSize)
import Util.Misc(replaceAt,posMin,impossible)
import GHC.Float
import Vision(desp,estimateHomographyRaw)
import Data.Colour.Names as Col hiding (gray)


regionMarker :: (Image a, Drawable a) => Size -> (t -> a) -> IO t -> IO (IO (t, Polyline))
regionMarker sz g cam = do
    w <- evWindow initRegion "Region Marker" sz Nothing (mouseGen (acts sz) kbdQuit)
    return $ do
        thing <- cam
        c <- getW w
        inWin w $ do
            drawImage (g thing)
            pointCoordinates sz
            shRegion c
        return (thing, c)
  where
    acts sz = ((MouseButton LeftButton, Down, modif), f sz)
              : map ch "1234"
        where ch c = ((Char c, Down, modif), h c sz)
    f sz pos (Closed ps) = Closed (replaceAt [k] [clickedPoint] ps)
        where clickedPoint = sel sz pos
              k = posMin $ map (distPoints clickedPoint) ps
    f _ _ _ = impossible "Open polyline in regionMarker"

    sel sz (Position c r) = head $ pixelsToPoints sz [Pixel (fromIntegral r) (fromIntegral c)]

    h c sz pos (Closed ps)
        | k `elem` [0 .. length ps - 1] = Closed (replaceAt [k] [sel sz pos] ps)
        | otherwise                     = (Closed ps)
      where k = fromEnum c - fromEnum '1'

    initRegion = transPol (desp (-0.5,-0.5)) $ Closed [Point 1 1,  Point 0 1, Point 0 0, Point 1 0]

    shRegion (Closed c) = do
        setColor' orange
        lineWidth $= 1
        renderPrimitive LineLoop $ mapM_ vertex c
        setColor' red
        sequence_ (zipWith textAt c (map return "1234"))

    textAt (Point x y) s = text2D (d x) (d y) s
      where d = double2Float

----------------------------------------------------------------------


$(autoParam "AspectRatioParam" "ar-"
  [( "aspectRatio","Double" ,realParam (4/3) 0 3),
   ( "autoRatio"  ,"String" ,stringParam "Manual" ["Auto","Manual"])]
 )



rectifyRegion 
     :: GImg pixel image 
     => (t -> image)                -- ^ selector
     -> Int                         -- ^ width
     -> AspectRatioParam
     -> (t, Polyline)
     -> (t, image)
rectifyRegion f w AspectRatioParam{..} (im,c) = (im,rim)
  where
    h = estimateHomographyRaw [[1,r],[-1,r],[-1,-r],[1,-r]] (g c)
    rim = warp zeroP sz h (f im)
    g (Closed ps) = map (\(Point x y) -> [x,y]) ps
    r' = distPoints (ps!!1) (ps!!2) / distPoints (ps!!0) (ps!!1) where (Closed ps) = c
    sz = (Size h' w)
    h' = round(fromIntegral w * r)
    r = if autoRatio == "Auto" then r' else recip aspectRatio

----------------------------------------------------------------------
