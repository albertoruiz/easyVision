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
( Drawable(..), drawImage', drawImage''
, drawTexture
, renderSignal
, renderAxes
, drawROI
, drawCamera
, cameraView
, drawInterestPoints
, drawVector
, extractSquare
, newTrackball
, captureGL
, limitSize
, drawContourLabeled
, viewPoint
, points'
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
import Vision.GUI.Trackball
import qualified Data.Colour.RGBSpace as Col
import Data.Colour.SRGB hiding (RGB)
import Data.Colour
import Data.Colour.Names
import Control.Monad(when)
import GHC.Float(double2Float)


-- | Types of images that can be shown in a window
class Drawable a where
    -- | Draws an image in the current window (showing the ROI)
    drawImage :: a -> IO ()

instance Drawable ImageGray where
    drawImage = drawImageGray

instance Drawable ImageRGB where
    drawImage = drawImageRGB

instance Drawable ImageFloat where
    drawImage = drawImageFloat

instance Drawable ImageYUV where
    drawImage = drawImageYUV

-- | The same as 'drawImage', but adapting window size to that of the image
drawImage' :: (Drawable a, Image a) => a -> IO ()
drawImage' im = do
    szW <- get windowSize
    let szI = glSize (size im)
    when (szW /= szI) $ windowSize $= szI >> postRedisplay Nothing
    drawImage im

-- | The same as 'drawImage'', but with a maximum window size
drawImage'' :: (Drawable a, Image a) => Int -> a -> IO ()
drawImage'' mx im = do
    szW <- get windowSize
    let szI = glSize (limitSize mx $ size im)
    when (szW /= szI) $ windowSize $= szI >> postRedisplay Nothing
    drawImage im

limitSize :: Int -> Size -> Size
limitSize mx (Size h w)
    | s <= mx = (Size h w)
    | otherwise = (Size h' w')
  where
    s = max h w
    r = fromIntegral w /  fromIntegral h
    (h',w') | w > h     = (round (fromIntegral mx/r), mx)
            | otherwise = (mx, round (fromIntegral mx*r))


pstart im = starting im (vroi im)

szgl = glSize .roiSize . vroi

-- GL.Size (fromIntegral $ step im `quot` (datasize im * layers im))
--                  (fromIntegral $ height $ isize im)


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


-- | Draws an image in the current window.
drawImageAux :: Img -> IO ()
drawImageAux m = do
    matrixMode $= Projection
    loadIdentity
    let w = width $ isize m
    let h = height $ isize m
    ortho2D (0) (0.0001+fromIntegral w-1::GLdouble) (0) (0.0001+fromIntegral h-1)
    matrixMode $= Modelview 0
    loadIdentity
    let r = fromIntegral $ r1 $ vroi m
    let c = fromIntegral $ c1 $ vroi m
    rasterPos (Vertex2 (c+0::GLfloat) (fromIntegral h-r-1.0001))
    GL.Size vw vh <- get windowSize
    pixelZoom $= (fromIntegral vw/ fromIntegral w,- fromIntegral vh/ fromIntegral h)
    --pixelZoom $= (1,-1)
    myDrawPixels m
    touchForeignPtr (fptr m)
    let r = vroi m
    pixelCoordinates (isize m)
    setColor 1 1 1
    lineWidth $= 1
    drawROI r

drawImageFloat (F im) = drawImageGray $ toGray (F im) -- drawImageAux im
drawImageGray (G im) = drawImageAux im
drawImageRGB (C im) = drawImageAux im
drawImageYUV (Y im) = drawImageRGB $ yuvToRGB (Y im)  -- drawImageAux im

drawROI r = renderPrimitive LineLoop $ mapM_ vertex
    [ Pixel (r1 r) (c1 r), Pixel (1+r2 r) (c1 r),
      Pixel (1+r2 r) (1+c2 r), Pixel (r1 r) (1+c2 r) ]

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

-- | Takes a centered square from an image and resizes it to the desired size (useful to obtain an image texture appropriate for 'drawCamera').
extractSquare :: Int -> ImageFloat -> ImageFloat
extractSquare sz (F im) = resize (Size sz sz) (F im {vroi = roi}) where
    w = width $ isize im
    h = height $ isize im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}

---------------------------------------------------------
-- deprecated
drawInterestPoints' :: Size -> [InterestPoint] -> IO ()
drawInterestPoints' sz ipts = do
    pointCoordinates sz
    renderPrimitive Points (mapM_ drawPts ipts)
    renderPrimitive Lines  (mapM_ drawOris ipts)
    mapM_ drawSample ipts
  where
    drawOris IP {ipPosition = p@(Point x y), ipOrientation = a} = do
        vertex $ p
        vertex $ Point (x-0.05*cos a) (y+0.05*sin a)
    drawPts IP {ipPosition = p@(Point x y)} = do
        vertex $ p
    drawSample IP {ipPosition = p@(Point x y), ipScale = rad} = renderPrimitive LineLoop (mapM_ vertex (circle x y 16 rad))

circle x y n rad = [Point (x+rad*cos ang) (y+rad*sin ang) | ang <- [0, d .. 2*pi-d]]
    where d = 2*pi/fromIntegral n
------------------------------------------------------------

drawInterestPoints :: [InterestPoint] -> IO ()
drawInterestPoints ps = mapM_ shIP ps

shIP (IP (Point x y) s o _) = do
    renderPrimitive LineLoop box
  where
    m = desp (x,y) <> rot3 o <> desp (-x,-y)
    ps = ht m [[x-s,y-s],[x-s,y+s],[x+s,y+s],[x+s,y-s]]
    box = mapM_ vertex ps

shIP' (IP (Point x y) s o _) = do
    renderPrimitive LineLoop box
    renderPrimitive Lines dir
  where
    box =  vertex (Point (x-s) (y-s))
        >> vertex (Point (x-s) (y+s))
        >> vertex (Point (x+s) (y+s))
        >> vertex (Point (x+s) (y-s))
    dir = vertex (Point (x) (y))
        >> vertex (Point (x-s*cos o) (y+s*sin o))

-----------------------------------------------------

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

renderSignal ls = do
    let delta = 1.8/fromIntegral (length ls-1)
    let points = zipWith Point [0.9, 0.9-delta ..] ls
    GL.renderPrimitive GL.LineStrip $ mapM_ GL.vertex points

renderAxes = 
    GL.renderPrimitive GL.Lines $ mapM_ GL.vertex
        [Point (-1) 0, Point 1 0, Point 0 (-1), Point 0 1]

--------------------------------------------------------------------------

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

instance Renderable [Point] where
  render = renderPrimitive Points . mapM_ vertex

instance Renderable Point where
  render p = render [p]

instance Renderable (Vector Double) where
    render v = renderPrimitive LineStrip (vertex $ fromColumns [t,v])
      where
        t = linspace (dim v) (0.9,-0.9)

instance Renderable HLine where
    render (HLine a 0 c) = render $ Open [Point x (-3), Point x 3]
      where x = -c/a
    render (HLine a b c) = render $ Open [ p (-2), p 2 ]
      where p x = Point x ((-a*x-c)/b)

instance Renderable [HLine] where
    render = mapM_ render

instance Renderable [Segment] where
  render = renderPrimitive Lines . mapM_ vertex

instance Renderable Segment where
  render s = render [s]

points' :: [Point] -> Drawing
points' = Draw

------------------------------------------------------------


drawContourLabeled :: Colour Float -> Colour Float -> Colour Float -> Float -> Float -> Polyline -> Drawing
drawContourLabeled cl cp ct wd sz cont = Draw [
      lineWd wd . color cl $ cont,
      color cp . pointSz sz $ c,
      color ct $ (zipWith (textF Helvetica10) c (map show [(0::Int)..])) 
    ]
  where
    c = polyPts cont


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

