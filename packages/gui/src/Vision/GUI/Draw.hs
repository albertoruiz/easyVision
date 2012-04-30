{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

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
, extractSquare
, captureGL
, drawContourLabeled
, drawPointsLabeled
, drawPoints3DLabeled
, drawPointCoords
, viewPoint
, lineStrip, axes3D, text3DAtF
, drawPolygon
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point,color)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc(resize,yuvToRGB,toGray)
import Data.IORef
import Foreign (touchForeignPtr,castPtr)
import Numeric.LinearAlgebra hiding (step)
import Vision
import Util.Rotation
import Util.Misc(degree,debug,Mat)
import Vision.GUI.Types
import qualified Data.Colour.RGBSpace as Col
import Data.Colour.SRGB hiding (RGB)
import Data.Colour
import Data.Colour.Names
import Control.Monad(when)
import GHC.Float(double2Float)
import Util.Geometry(HPoint(..),Point3D(..),HPoint3D(..),HLine3D(..),HPlane(..),Meet(..))
import Text.Printf(printf)


pstart im = starting im (vroi im)

szgl = glSize .roiSize . vroi


myDrawPixels m@Img{itype=RGB} = do
    GL.rowLength Unpack $= fromIntegral (step m `quot` (datasize m * layers m))
    GL.drawPixels (szgl m) (PixelData GL.RGB UnsignedByte (pstart m))

myDrawPixels m@Img{itype=Gray} = do
    GL.rowLength Unpack $= fromIntegral (step m `quot` (datasize m * layers m))
    GL.drawPixels (szgl m) (PixelData Luminance UnsignedByte (pstart m))

myDrawPixels m@Img{itype=I32f} = do
    GL.rowLength Unpack $= fromIntegral (step m `quot` (datasize m * layers m))
    GL.drawPixels (szgl m) (PixelData Luminance Float (pstart m))

myDrawPixels m@Img{itype=YUV} = error "myDrawPixels undefined for YUV"


--------------------------------------------------------------------------------

-- | Draws an image 32f as a texture in the current window, in the desired 3D coordinates corresponding to (0,0), (1,0), (1,1), (0,1). (Drawing is very fast if the sizes are powers of 2.)
drawTexture' :: ImageFloat -> [[Double]] -> IO ()
drawTexture' (F im) [v1,v2,v3,v4] = do
    texImage2D  Nothing
                NoProxy
                0
                Luminance'
                (TextureSize2D (fromIntegral $ width $ isize im) (fromIntegral $ height $ isize im))
                0
                (PixelData Luminance Float (ptr im))

    touchForeignPtr (fptr im)
    texture Texture2D $= Enabled
    renderPrimitive Polygon $ do
        vert (TexCoord2 0 0) v1
        vert (TexCoord2 1 0) v2
        vert (TexCoord2 1 1) v3
        vert (TexCoord2 0 1) v4
    texture Texture2D $= Disabled

  where
    vert :: TexCoord2 GLdouble -> [Double] -> IO ()
    vert t p = do
          multiTexCoord (TextureUnit 0) t
          vertex p

------------------------------------------------------------

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

{-# DEPRECATED extractSquare "use new drawing camera tools" #-}
extractSquare :: Int -> ImageFloat -> ImageFloat
extractSquare sz (F im) = resize (Size sz sz) (F im {vroi = roi}) where
    w = width $ isize im
    h = height $ isize im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}

---------------------------------------------------------

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

------------------------------------------------------

{-
renderSignal ls = do
    let delta = 1.8/fromIntegral (length ls-1)
    let points = zipWith Point [0.9, 0.9-delta ..] ls
    GL.renderPrimitive GL.LineStrip $ mapM_ GL.vertex points

renderAxes = 
    GL.renderPrimitive GL.Lines $ mapM_ GL.vertex
        [Point (-1) 0, Point 1 0, Point 0 (-1), Point 0 1]

-- | representation of the elements of a vector as vertical bars from a starting position
drawVector :: Int -- ^ column
          -> Int -- ^ row
          -> Vector Double -- ^ input vector
          -> IO ()
drawVector x y v = do
    let f k = vertex (Vertex2 x1 y1) >> vertex (Vertex2 x2 y2)
            where x1 = fromIntegral (x+k) :: GLint
                  y1 = fromIntegral y
                  x2 = x1
                  y2 = y1 + round (-v@>k)
    renderPrimitive Lines $ do
        mapM_ f [0..dim v -1]
        vertex (Pixel y x) >> vertex (Pixel y (x+dim v -1))
-}

--------------------------------------------------------------------------------

-- | captures the contents of the current opengl window (very slow)
captureGL :: IO ImageRGB
captureGL = do
    sz <- get windowSize
    img <- image (evSize sz)
    let C (Img {ptr = p, fptr = f}) = img
    when (width (size img) `rem` 32 /= 0) $ putStrLn "Warning, captureGL with wrong padding"
    readPixels (Position 0 0) sz (PixelData GL.RGB UnsignedByte p)
    touchForeignPtr f
    return img

----------------------------------------------------------------

renderImageIn :: EVWindow st -> Img -> IO ()
renderImageIn evW m = do
    policy <- readIORef (evPolicy evW)
    prefSize <- readIORef (evPrefSize evW)
    let imSize@(Size h w) = isize m
        szI = limitSize 800 imSize
    when (prefSize == Nothing || policy == DynamicSize) $
        writeIORef (evPrefSize evW) (Just szI)
    szW@(GL.Size vw vh) <- get windowSize
    Just szP <-readIORef (evPrefSize evW)
    let szT = if policy == UserSize then szW else glSize szP
    when (szW /= szT) $ windowSize $= szT >> postRedisplay Nothing
    when (prefSize == Nothing && policy == StaticSize) $
       writeIORef (evPolicy evW) (UserSize)
    
    (z',_,_) <- readIORef (evZoom evW)
    
    let zw = fromIntegral vw/ fromIntegral w
        z = (floatGL . double2Float) z' * zw
        r@ROI {..} = vroi m
        szTe@(Size th tw) = evSize szT
        r0 = round (fromIntegral r1 * zw + (fromIntegral th-fromIntegral h*zw)/2)
        c0 = round (fromIntegral c1 * zw)
        [Point x0 y0] =pixelsToPoints szTe [Pixel (max 0 r0) c0]
        roipts = pixelsToPoints (imSize) [ Pixel r1 c1, Pixel (1+r2) c1,
                                           Pixel (1+r2) (1+c2), Pixel r1 (1+c2) ]
    rasterPos (Vertex2 (doubleGL x0) (doubleGL y0-1E-6))
    pixelZoom $= (z,-z)   
    myDrawPixels m { vroi = r { r1 = r1 - min 0 (round $ (fromIntegral r0)/zw) } }
    touchForeignPtr (fptr m)
    render $ Draw [color white . lineWd 1 $ (Closed roipts)]

--------------------------------------------------------------------------------

instance Renderable ImageGray where
    renderIn w (G im) = renderImageIn w im

instance Renderable ImageRGB where
    renderIn w (C im) = renderImageIn w im

instance Renderable ImageFloat where
    renderIn w = renderIn w . toGray

instance Renderable ImageYUV where
    renderIn w = renderIn w . yuvToRGB

instance Renderable Polyline where
    render (Closed ps) = renderPrimitive LineLoop (vertex (Closed ps))
    render (Open ps) = renderPrimitive LineStrip (vertex (Open ps))

instance Renderable [Polyline] where
    render = mapM_ render

instance Renderable (Vector Double) where
    render v = renderPrimitive LineStrip (vertex $ fromColumns [t,v])
      where
        t = linspace (dim v) (0.9,-0.9)

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


lineStrip :: Vertex a => [a] -> Drawing
lineStrip = Raw . GL.renderPrimitive GL.LineStrip . mapM_ GL.vertex

drawPolygon :: Vertex a => [a] -> Drawing
drawPolygon = Raw . GL.renderPrimitive GL.Polygon . mapM_ GL.vertex

------------------------------------------------------------

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

viewPoint :: (Image im, Renderable im, Renderable x) => Mat -> Maybe im -> x -> Drawing
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

