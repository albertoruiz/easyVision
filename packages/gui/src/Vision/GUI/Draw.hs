{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Draw
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Drawing utilities.

-}
-----------------------------------------------------------------------------

module Vision.GUI.Draw
( drawTexture
, drawCamera
, cameraView
, captureGL
, drawContourLabeled
, drawPointsLabeled
, drawPoints3DLabeled
, drawPointCoords
, viewPoint
, lineStrip, axes3D, text3DAtF
, drawPolygon
, fillPolygon
, fillConvexPolygon
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point, color, Polygon)
import qualified Graphics.UI.GLUT as GL
import Image.Devel as Image
import Util.Geometry
--import ImagProc(resize,yuvToRGB, yCbCrToRGB, toGray,Channels(..),histogramN,blockImage)
import Data.IORef
import qualified Numeric.LinearAlgebra.HMatrix as LA
import Numeric.LinearAlgebra.HMatrix as LA hiding (step,size)
import Util.Camera(toCameraSystem)
import Vision.GUI.Objects(cameraOutline)
import Util.Homogeneous(ht,desp)
import Util.Rotation
import Util.Misc(Mat)
import Vision.GUI.Types
import Data.Colour.SRGB hiding (RGB)
import Data.Colour.Names
import Control.Monad(when)
import GHC.Float(double2Float)
import Util.Polygon(convexComponents)
import Text.Printf(printf)

szgl :: Image t -> GL.Size
szgl = glSize .roiSize . roi


myDrawPixels :: PixelFormat -> DataType -> Int -> Image t -> IO ()
myDrawPixels m t s img@Image{..} = withImage img $ do
    GL.rowLength Unpack $= fromIntegral (step `quot` s)
    GL.drawPixels (szgl img) (PixelData m t (starting img))

--------------------------------------------------------------------------------

-- | Draws an image 32f as a texture in the current window, in the desired 3D coordinates corresponding to (0,0), (1,0), (1,1), (0,1). (Drawing is very fast if the sizes are powers of 2.)
drawTexture' :: ImageFloat -> [[Double]] -> IO ()
drawTexture' im [v1,v2,v3,v4] = do
  withImage im $ do
    texImage2D  Texture2D
                NoProxy
                0
                Luminance'
                (TextureSize2D (fromIntegral w) (fromIntegral h))
                0
                (PixelData Luminance Float (starting im))
    texture Texture2D $= Enabled
    renderPrimitive GL.Polygon $ do
        vert (TexCoord2 0 0) v1
        vert (TexCoord2 1 0) v2
        vert (TexCoord2 1 1) v3
        vert (TexCoord2 0 1) v4
    texture Texture2D $= Disabled

  where
    Size h w = Image.size im
    vert :: TexCoord2 GLdouble -> [Double] -> IO ()
    vert t p = do
          multiTexCoord (TextureUnit 0) t
          vertex p

drawTexture' _ _ = error "drawTexture' requires 4 vertexes"

------------------------------------------------------------

-- | sets the opengl view of a given camera matrix
cameraView :: Matrix Double -- ^ 3x4 camera matrix (with K=diag f f 1)
           -> Double        -- ^ aspect ratio of the window (e.g., 4\/3)
           -> Double        -- ^ distance to nearest object to show (e.g., 0.1)
           -> Double        -- ^ distance to farthest object to show (e.g., 100)
           -> IO ()
cameraView m ar near far = do
    let (p,f) = toCameraSystem m
    matrixMode $= Projection
    loadIdentity
    let fov = 2* atan (1/(f*ar)) / degree
    perspective (doubleGL fov) (doubleGL ar) (doubleGL near) (doubleGL far)
    lookAt (Vertex3 0 0 0)
           (Vertex3 0 0 1)
           (Vector3 0 1 0)
    matrixMode $= Modelview 0
    loadIdentity
    mat <- newMatrix RowMajor (map doubleGL $ toList $ flatten $ inv p) :: IO (GLmatrix GLdouble)
    multMatrix mat


--------------------------------------------------------------------------------

-- | captures the contents of the current opengl window (very slow)
captureGL :: IO ImageRGB
captureGL = do
    sz@(GL.Size w h) <- get windowSize
    img <- newImage undefined (evSize sz)
    let (ps,_c) = rowPtrs img
        f k ptr = readPixels (Position 0 k) (GL.Size w 1) (PixelData GL.RGB UnsignedByte ptr)
    withImage img $ sequence_ $ zipWith f [h-1, h-2 ..] ps
    return img

----------------------------------------------------------------

renderImageIn :: EVWindow st -> PixelFormat -> DataType -> Int -> Image t -> IO ()
renderImageIn evW m t s img = do
    policy <- readIORef (evPolicy evW)
    prefSize <- readIORef (evPrefSize evW)
    let imSize@(Size h w) = Image.size img
        szI = limitSize 800 imSize
    when (prefSize == Nothing || policy == DynamicSize) $
        writeIORef (evPrefSize evW) (Just szI)
    szW@(GL.Size vw _vh) <- get windowSize
    Just szP <-readIORef (evPrefSize evW)
    let szT = if policy == UserSize then szW else glSize szP
    when (szW /= szT) $ windowSize $= szT >> postRedisplay Nothing
    when (prefSize == Nothing && policy == StaticSize) $
       writeIORef (evPolicy evW) (UserSize)
    
    (z',_,_) <- readIORef (evZoom evW)
    
    let zw = fromIntegral vw/ fromIntegral w
        z = (floatGL . double2Float) z' * zw
        ROI r1 r2 c1 c2 = roi img
        szTe@(Size th _tw) = evSize szT
        r0 = round (fromIntegral r1 * zw + (fromIntegral th-fromIntegral h*zw)/2)
        c0 = round (fromIntegral c1 * zw)
        [Point x0 y0] =pixelsToPoints szTe [Pixel (max 0 r0) c0]
        roipts = pixelsToPoints (imSize) [ Pixel r1 c1, Pixel (1+r2) c1,
                                           Pixel (1+r2) (1+c2), Pixel r1 (1+c2) ]
    rasterPos (Vertex2 (doubleGL x0) (doubleGL y0-1E-6))
    pixelZoom $= (z,-z)
    GL.clearColor $= Color4 0.2 0 0 0
    when (roiArea (roi img) > 0) $ do
        myDrawPixels m t s img { roi = ROI (r1 - min 0 (round $ (fromIntegral r0)/zw)) r2 c1 c2 }
        render $ Draw [color white . lineWd 1 $ (Closed roipts)]

--------------------------------------------------------------------------------

instance Renderable (Image I8u) where
    renderIn w = renderImageIn w Luminance UnsignedByte 1

instance Renderable (Image RGB) where
    renderIn w = renderImageIn w GL.RGB UnsignedByte 3


instance Renderable Polyline where
    render (Closed ps) = renderPrimitive LineLoop (vertex (Closed ps))
    render (Open ps) = renderPrimitive LineStrip (vertex (Open ps))

instance Renderable Polygon where
    render (Polygon ps) = renderPrimitive LineLoop (mapM_ vertex ps)

instance Renderable [Polyline] where
    render = mapM_ render

instance Renderable [Polygon] where
    render = mapM_ render

instance Renderable (Vector Double) where
    render v = renderPrimitive LineStrip (vertex $ fromColumns [t,v])
      where
        t = linspace (LA.size v) (0.9,-0.9)

instance Renderable [Point] where
  render = renderPrimitive Points . mapM_ vertex

instance Renderable Point where
  render p = render [p]

instance Renderable [HPoint] where
  render = renderPrimitive Points . mapM_ vertex

instance Renderable HPoint where
  render p = render [p]

instance Renderable [Point3D] where
  render = renderPrimitive Points . mapM_ vertex

instance Renderable Point3D where
  render p = render [p]

instance Renderable [HPoint3D] where
  render = renderPrimitive Points . mapM_ vertex

instance Renderable HPoint3D where
  render p = render [p]

instance Renderable HLine where
    render (HLine a 0 c) = render $ Open [Point x (-3), Point x 3]
      where x = -c/a
    render (HLine a b c) = render $ Open [ p (-2), p 2 ]
      where p x = Point x ((-a*x-c)/b)

instance Renderable [HLine] where
    render = mapM_ render

instance Renderable HLine3D where
  render l = render $ lineStrip [p1,p2]
    where
      p1 = meet l (HPlane 1 0.1   0   20)
      p2 = meet l (HPlane 1 0   0.2 (-20))

instance Renderable [HLine3D] where
    render = mapM_ render


instance Renderable [Segment] where
  render = renderPrimitive Lines . mapM_ vertex

instance Renderable Segment where
  render s = render [s]


shIP :: InterestPoint -> IO ()
shIP (IP (Point x y) s o _) = do
    renderPrimitive LineLoop box
  where
    m = desp (x,y) <> rot3 o <> desp (-x,-y)
    ps = ht m [[x-s,y-s],[x-s,y+s],[x+s,y+s],[x+s,y-s]]
    box = mapM_ vertex ps

instance Renderable InterestPoint where
  render = shIP

instance Renderable [InterestPoint] where
  render = mapM_ render


instance Renderable KeyPoint where
  render = shKP

instance Renderable [KeyPoint] where
  render = mapM_ render

shKP :: KeyPoint -> IO ()
shKP (KeyPoint (Point x y) s' o) = do
    GL.renderPrimitive GL.LineStrip box
  where
    s = s'/2
    m = unsafeFromMatrix (desp (x,y) <> rot3 o) :: Homography
    ps = m <| [HPoint 0 0 1, HPoint (-s) 0 1, HPoint (-s) (-s) 1, HPoint (-s) (s) 1, HPoint (s) (s) 1, HPoint(s) (-s) 1, HPoint (-s) (-s) 1, HPoint (-s) 0 1]
    box = mapM_ GL.vertex ps




lineStrip :: Vertex a => [a] -> Drawing
lineStrip = Raw . GL.renderPrimitive GL.LineStrip . mapM_ GL.vertex

drawPolygon :: Vertex a => [a] -> Drawing
drawPolygon = Raw . GL.renderPrimitive GL.Polygon . mapM_ GL.vertex

fillPolygon :: Polygon -> Drawing
fillPolygon p = Draw $ map fillConvexPolygon (convexComponents p)

fillConvexPolygon :: Polygon -> Drawing
fillConvexPolygon = drawPolygon . polygonNodes

------------------------------------------------------------

axes3D :: Double -> Drawing
axes3D l = Draw [ lineStrip
                    [ Point3D 0 0 0
                    , Point3D l 0 0
                    , Point3D 0 0 0
                    , Point3D 0 l 0
                    , Point3D 0 0 0
                    , Point3D 0 0 l ]
                , text3DAtF Helvetica12 (Point3D (l*1.1) 0 0) "x"
                , text3DAtF Helvetica12 (Point3D 0 (l*1.1) 0) "y"
                , text3DAtF Helvetica12 (Point3D 0 0 (l*1.1)) "z"
                ]


text3DAtF :: Font a => a -> Point3D -> String -> Drawing
text3DAtF f (Point3D x y z) s = Raw $ do
    GL.rasterPos (GL.Vertex3 (doubleGL x) (doubleGL y) (doubleGL z))
    GL.renderString f s

------------------------------------------------------------

-- FIXME function names

drawContourLabeled :: Colour Float -> Colour Float -> Colour Float -> Float -> Float -> Polyline -> Drawing
drawContourLabeled cl cp ct wd sz cont = Draw [
      lineWd wd . color cl $ cont,
      color cp . pointSz sz $ c,
      color ct $ (zipWith (textF Helvetica10) c (map show [(0::Int)..])) 
    ]
  where
    c = polyPts cont


drawPointsLabeled :: [Point] -> Drawing
drawPointsLabeled pts = pointSz 3 $
    [ Draw pts
    , draws $ zipWith (textF Helvetica10) pts (map ((' ':).show) [(0::Int)..])]

drawPoints3DLabeled :: [Point3D] -> Drawing
drawPoints3DLabeled pts = pointSz 3 $
    [ Draw pts
    , draws $ zipWith (text3DAtF Helvetica10) pts (map ((' ':).show) [(0::Int)..])]

drawPointCoords :: [Point] -> Drawing
drawPointCoords ps = Draw [ --Raw (logicOp $= (Just Xor)),
                            Draw (map g ps), pointSz 3 ps
                          --, Raw (logicOp $= (Just Copy))
                          ]
  where
    g p@(Point x y) = textF Helvetica10 p (printf "  (%.2f,  %.2f)" x y)

--------------------------------------------------------------------------------

viewPoint :: (Renderable (Image t), Renderable x) => Mat -> Maybe (Image t) -> x -> Drawing
-- ^ draw 3D objects as seen from a given camera matrix, with an optional image background
viewPoint cam mbimg things = Draw [
    Raw (depthFunc $= Just Less >> clear [DepthBuffer]),
    case mbimg of
        Just img -> Draw [Draw img, Raw $ clear [DepthBuffer]]
        Nothing  -> Draw (),
    Raw $ cameraView cam (4/3) 0.1 100,
    Draw things,
    Raw $ depthFunc $= Nothing ]


drawTexture :: ImageFloat -> [[Double]] -> Drawing
-- ^ provisional
drawTexture x ps = Raw (drawTexture' x ps)

drawCamera :: Double -> Matrix Double -> Maybe ImageFloat -> Drawing
-- ^ provisional
drawCamera sz cam mbt = Raw (drawCamera' sz cam mbt)



-- | It shows the outline of a camera and an optional image (texture) in its image plane.
drawCamera' :: Double -> Matrix Double -> Maybe ImageFloat -> IO ()
drawCamera' size cam Nothing = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (fromList[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    renderPrimitive LineLoop $ mapM_ vertex outline

drawCamera' size cam (Just imgtext) = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (fromList[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    let q = 0.95 --0.75                     TO DO: fix this
    drawTexture' imgtext $ ht m
              [[ q,  q, f],
               [-q,  q, f],
               [-q, -q, f],
               [ q, -q, f]]
    renderPrimitive LineLoop $ mapM_ vertex outline

--------------------------------------------------------------------------------

