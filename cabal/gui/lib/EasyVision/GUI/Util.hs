{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.GUI.Util
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
-}
-----------------------------------------------------------------------------

module EasyVision.GUI.Util
( pointCoordinates, pointCoords
, pixelCoordinates, pixelCoords
, setColor, setColor'
, text2D, textAt, textAtF
, evSize, glSize
, floatGL, doubleGL
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point)
import qualified Graphics.UI.GLUT as GL
import ImagProc.Ipp.Core
import ImagProc(resize,yuvToRGB,toGray)
import Data.IORef
import Foreign (touchForeignPtr,castPtr)
import Numeric.LinearAlgebra hiding (step)
import Vision
import Util.Rotation
import Util.Misc(degree)
import qualified Data.Colour.RGBSpace as Col
import Data.Colour.SRGB hiding (RGB)
import Data.Colour
import Control.Monad(when)
--import Features(Polyline(..))
import GHC.Float(double2Float)
import Unsafe.Coerce(unsafeCoerce)

------------------------------------------------------------

-- | Sets the current color to the given R, G, and B components.
setColor :: Float -> Float -> Float -> IO ()
setColor r g b = currentColor $= Color4 (floatGL r) (floatGL g) (floatGL b) 1

-- | Sets the current color to the given R, G, and B components.
setColor' :: Colour Float -> IO ()
setColor' c = setColor r g b where Col.RGB {Col.channelRed = r, Col.channelGreen = g, Col.channelBlue = b} = toSRGB c

-- | Sets ortho2D to draw 2D normalized points in a right handed 3D system (x from -1 (left) to +1 (right) and y from -1 (bottom) to +1 (top)).
pointCoordinates :: Size -> IO()
pointCoordinates (Size h w) = draw2Dwith (ortho2D 1 (-1) (-r) r)
    where r = fromIntegral h / fromIntegral w

pointCoords :: IO ()
pointCoords = get windowSize >>= pointCoordinates . evSize

-- | Sets ortho2D to draw 2D unnormalized pixels as x (column, 0 left) and y (row, 0 top).
pixelCoordinates :: Size -> IO()
pixelCoordinates (Size h w) = draw2Dwith (ortho2D eps (fromIntegral w -eps) (fromIntegral h - eps) eps)
    where eps = 0.0001

pixelCoords :: IO ()
pixelCoords = get windowSize >>= pointCoordinates . evSize

draw2Dwith ortho = do
    matrixMode $= Projection
    loadIdentity
    ortho
    matrixMode $= Modelview 0
    loadIdentity

----------------------------------------------------------------------

{-
instance VertexComponent Double where
  vertex2 = undefined

instance VertexComponent Float

instance RasterPosComponent Double

instance RasterPosComponent Float
-}

----------------------------------------------------------------------

instance Vertex Pixel where
    vertex (Pixel r c) = vertex (Vertex2 (fromIntegral c) (fromIntegral r::GLint))
    vertexv = undefined

instance Vertex Point where
    vertex (Point x y) = vertex (Vertex2 (doubleGL x) (doubleGL y))
    vertexv = undefined

instance Vertex [Double] where
    vertex [x,y]   = vertex (Vertex2 (doubleGL x) (doubleGL y))
    vertex [x,y,z] = vertex (Vertex3 (doubleGL x) (doubleGL y) (doubleGL z))
    vertex _  = error "vertex on list without two or three elements"
    vertexv = undefined

instance Vertex (Complex Double) where
    vertex (x:+y) = vertex (Vertex2 (doubleGL x) (doubleGL y))
    vertexv = undefined

instance Vertex Segment where
    vertex s = do
        vertex $ (extreme1 s)
        vertex $ (extreme2 s)
    vertexv = undefined

instance Vertex (Vector Double) where
    vertex v | dim v == 2 = vertex (Vertex2 (doubleGL $ v@>0) (doubleGL $ v@>1))
             | dim v == 3 = vertex (Vertex3 (doubleGL $ v@>0) (doubleGL $ v@>1) (doubleGL $ v@>2))
    vertexv = undefined

instance Vertex (Vector (Complex Double)) where
    vertex = mapM_ vertex . toList
    vertexv = undefined

instance Vertex (Matrix Double) where
    vertex = mapM_ vertex . toRows
    vertexv = undefined

instance Vertex Polyline where
    vertex = mapM_ vertex . polyPts
    vertexv = undefined


----------------------------------------------------------------------

text2D x y s = do
    rasterPos (Vertex2 (floatGL x) (floatGL y))
    renderString Helvetica12 s

textAt = textAtF Helvetica12
    
textAtF f (Point x y) s = do
    rasterPos (Vertex2 (doubleGL x) (doubleGL y))
    renderString f s

----------------------------------------------------------------

-- we should use only one size type
-- | converts an OpenGL Size into a 'Size'
evSize :: GL.Size -> Size
evSize (GL.Size w h) = Size    (t h) (t w) where t = fromIntegral.toInteger

-- | converts a 'Size' into an OpenGL Size.
glSize :: Size -> GL.Size
glSize (Size    h w) = GL.Size (t w) (t h) where t = fromIntegral.toInteger

----------------------------------------------------------------------

doubleGL :: Double -> GLdouble
doubleGL = unsafeCoerce -- realToFrac

floatGL :: Float -> GLfloat
floatGL = unsafeCoerce -- realToFrac

