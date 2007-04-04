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
, drawCamera
, drawInterestPoints
, extractSquare
, newTrackball
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc.Saddle
import ImagProc.ImageProcessing(resize32f,yuvToRGB)
import Data.IORef
import Foreign (touchForeignPtr)
import GSL hiding (size)
import Vision
import EasyVision.Trackball


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

myDrawPixels m@Img{itype=RGB} = 
    GL.drawPixels (GL.Size (fromIntegral $ step m `quot` 3) (fromIntegral $ height $ isize m))
                  (PixelData GL.RGB UnsignedByte (ptr m))

myDrawPixels m@Img{itype=Gray} = 
    GL.drawPixels (GL.Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height $ isize m))
                  (PixelData Luminance UnsignedByte (ptr m))

myDrawPixels m@Img{itype=I32f} = 
    GL.drawPixels (GL.Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height $ isize m))
                  (PixelData Luminance Float (ptr m))

myDrawPixels m@Img{itype=YUV} = 
    GL.drawPixels (GL.Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height $ isize m))
                  (PixelData CMYK UnsignedByte (ptr m))


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
    rasterPos (Vertex2 (0::GLfloat) (fromIntegral h-1.0001))
    GL.Size vw vh <- get windowSize
    pixelZoom $= (fromIntegral vw/ fromIntegral w,- fromIntegral vh/ fromIntegral h)
    --pixelZoom $= (1,-1)
    myDrawPixels m
    touchForeignPtr (fptr m)
    let r = shrink (-1,-1) $ vroi m
    pixelCoordinates (isize m)
    setColor 1 1 1
    lineWidth $= 1
    renderPrimitive LineLoop $ mapM_ vertex $
        Pixel (r1 r) (c1 r) :
        Pixel (r2 r) (c1 r) :
        Pixel (r2 r) (c2 r) :
        Pixel (r1 r) (c2 r) :[]

drawImageFloat (F im) = drawImage' im
drawImageGray (G im) = drawImage' im
drawImageRGB (C im) = drawImage' im
drawImageYUV (Y im) = yuvToRGB (Y im) >>= drawImageRGB -- drawImage' im

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
pixelCoordinates (Size h w) = draw2Dwith (ortho2D 0 (fromIntegral w-1) (fromIntegral h-1) 0)

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
extractSquare :: Int -> ImageFloat -> IO ImageFloat
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
    pixelCoordinates sz
    mapM_ drawSample ipts
  where 
    drawOris IP {ipPosition = p@(Point x y), ipOrientation = a} = do
        vertex $ p
        vertex $ Point (x+0.05*cos a) (y+0.05*sin a)
    drawPts IP {ipPosition = p@(Point x y)} = do
        vertex $ p
    drawSample IP {ipSample = pts} = renderPrimitive LineLoop (mapM_ vertex pts)