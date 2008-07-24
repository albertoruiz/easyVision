{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Draw
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

HOpenGL drawing utilities.

-}
-----------------------------------------------------------------------------

module EasyVision.Draw
( pointCoordinates
, pixelCoordinates
, Drawable(..)
, drawTexture
, setColor
, text2D
, renderSignal
, renderAxes
, drawROI
, drawCamera
, cameraView
, drawInterestPoints
, drawVector
, extractSquare
, newTrackball
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import Features.Matching
import ImagProc.ImageProcessing(resize32f,yuvToRGB)
import Data.IORef
import Foreign (touchForeignPtr,castPtr)
import Numeric.LinearAlgebra
import Vision
import EasyVision.Trackball
import EasyVision.Util


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
drawImage' :: Img -> IO ()
drawImage' m = do
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

drawImageFloat (F im) = drawImage' im
drawImageGray (G im) = drawImage' im
drawImageRGB (C im) = drawImage' im
drawImageYUV (Y im) = drawImageRGB $ yuvToRGB (Y im)  -- drawImage' im

drawROI r = renderPrimitive LineLoop $ mapM_ vertex
    [ Pixel (r1 r) (c1 r), Pixel (1+r2 r) (c1 r),
      Pixel (1+r2 r) (1+c2 r), Pixel (r1 r) (1+c2 r) ]

--------------------------------------------------------------------------------

-- | Draws an image 32f as a texture in the current window, in the desired 3D coordinates corresponding to (0,0), (1,0), (1,1), (0,1). (Drawing is very fast if the sizes are powers of 2.)
drawTexture :: ImageFloat -> [[Double]] -> IO ()
drawTexture (F im) [v1,v2,v3,v4] = do
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
    vert :: TexCoord2 GLdouble -> [GLdouble] -> IO ()
    vert t [x,y,z] = do
          multiTexCoord (TextureUnit 0) t
          vertex (Vertex3 x y z)

------------------------------------------------------------

-- | Sets the current color to the given R, G, and B components.
setColor :: Float -> Float -> Float -> IO ()
setColor r g b = currentColor $= Color4 r g (b::GLfloat) 1

-- | Sets ortho2D to draw 2D normalized points in a right handed 3D system (x from -1 (left) to +1 (right) and y from -1 (bottom) to +1 (top)).
pointCoordinates :: Size -> IO()
pointCoordinates (Size h w) = draw2Dwith (ortho2D 1 (-1) (-r) r)
    where r = fromIntegral h / fromIntegral w

-- | Sets ortho2D to draw 2D unnormalized pixels as x (column, 0 left) and y (row, 0 top).
pixelCoordinates :: Size -> IO()
pixelCoordinates (Size h w) = draw2Dwith (ortho2D eps (fromIntegral w -eps) (fromIntegral h - eps) eps)
    where eps = 0.0001

draw2Dwith ortho = do
    matrixMode $= Projection
    loadIdentity
    ortho
    matrixMode $= Modelview 0
    loadIdentity


instance Vertex Pixel where
    vertex (Pixel r c) = vertex (Vertex2 (fromIntegral c) (fromIntegral r::GLint))
    vertexv = undefined

instance Vertex Point where
    vertex (Point x y) = vertex (Vertex2 x y)
    vertexv = undefined

instance Vertex [Double] where
    vertex [x,y]   = vertex (Vertex2 x y)
    vertex [x,y,z] = vertex (Vertex3 x y z)
    vertex _  = error "vertex on list without two or three elements"
    vertexv = undefined

instance Vertex (Complex Double) where
    vertex (x:+y) = vertex (Vertex2 x y)
    vertexv = undefined

instance Vertex Segment where
    vertex s = do
        vertex $ (extreme1 s)
        vertex $ (extreme2 s)
    vertexv = undefined

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

-- | It shows the outline of a camera and an optional image (texture) in its image plane.
drawCamera :: Double -> Matrix Double -> Maybe ImageFloat -> IO ()
drawCamera size cam Nothing = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (fromList[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    renderPrimitive LineLoop $ mapM_ vertex outline

drawCamera size cam (Just imgtext) = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (fromList[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    let q = 0.95 --0.75                     TO DO: fix this
    drawTexture imgtext $ ht m
              [[ q,  q, f],
               [-q,  q, f],
               [-q, -q, f],
               [ q, -q, f]]
    renderPrimitive LineLoop $ mapM_ vertex outline

-- | Takes a centered square from an image and resizes it to the desired size (useful to obtain an image texture appropriate for 'drawCamera').
extractSquare :: Int -> ImageFloat -> ImageFloat
extractSquare sz (F im) = resize32f (Size sz sz) (F im {vroi = roi}) where
    w = width $ isize im
    h = height $ isize im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}

drawInterestPoints :: Size -> [InterestPoint] -> IO ()
drawInterestPoints sz ipts = do
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
    perspective fov ar near far
    lookAt (Vertex3 0 0 0)
           (Vertex3 0 0 1)
           (Vector3 0 1 0)
    matrixMode $= Modelview 0
    loadIdentity
    mat <- newMatrix RowMajor (toList $ flatten $ inv p) :: IO (GLmatrix GLdouble)
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
