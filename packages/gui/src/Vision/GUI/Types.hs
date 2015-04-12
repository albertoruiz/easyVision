{-# LANGUAGE FlexibleInstances, ExistentialQuantification, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Types
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
-}
-----------------------------------------------------------------------------

module Vision.GUI.Types
( 
-- * Window representation
   EVWindow(..), MoveStatus(..), ResizePolicy(..), PauseStatus(..), WinRegion, WStatus(..)
-- * Drawing abstraction
,  Renderable(..), Drawing(..)
,  color, text, textF, pointSz, lineWd, windowTitle , clearColor, draws, colorAlpha, blend, sRGB
-- * Tools
, pointCoordinates, pointCoords
, pixelCoordinates, pixelCoords
, setColor, setColor'
, text2D, textAt, textAtF
, evSize, glSize
, floatGL, doubleGL
, prepZoom, unZoom
, withOrtho2D
, winTitle
, setRegion
) where

import Graphics.UI.GLUT hiding (RGB, Matrix, Size, Point,color,clearColor,windowTitle,blend)
import qualified Graphics.UI.GLUT as GL
import Image hiding (RGB)
import Util.Geometry
import Numeric.LinearAlgebra.HMatrix as LA hiding (step)
import Data.Colour(Colour)
import Data.Colour.SRGB(RGB(..),toSRGB,sRGB)
import Unsafe.Coerce(unsafeCoerce)
import Data.IORef
import Control.Concurrent
import Image.ROI(poly2roi)

------------------------------------------------------------

-- | Sets the current color to the given R, G, and B components.
setColor :: Float -> Float -> Float -> IO ()
setColor r g b = currentColor $= Color4 (floatGL r) (floatGL g) (floatGL b) 1

-- | Sets the current color to the given R, G, and B components.
setColor' :: Colour Float -> IO ()
setColor' = render

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
pixelCoords = get windowSize >>= pixelCoordinates . evSize

draw2Dwith :: IO a -> IO ()
draw2Dwith ort = do
    matrixMode $= Projection
    loadIdentity
    _ <- ort
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

instance Vertex HPoint where
    vertex (HPoint x y w) = vertex (HPoint3D x y 0 w)
    vertexv = undefined

instance Vertex Point3D where
    vertex (Point3D x y z) = vertex (Vertex3 (doubleGL x) (doubleGL y) (doubleGL z))
    vertexv = undefined

instance Vertex HPoint3D where -- FIXME !??
    vertex (HPoint3D x y z w) | w > 0     = vertex (Vertex4 (doubleGL x) (doubleGL y) (doubleGL z) (doubleGL w))
                              | otherwise = vertex (Vertex4 (-doubleGL x) (-doubleGL y) (-doubleGL z) (-doubleGL w))
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
    vertex (Segment p q) = do
        vertex p
        vertex q
    vertexv = undefined

instance Vertex (Vector Double) where
    vertex v | LA.size v == 2 = vertex (Vertex2 (doubleGL $ v!0) (doubleGL $ v!1))
             | LA.size v == 3 = vertex (Vertex3 (doubleGL $ v!0) (doubleGL $ v!1) (doubleGL $ v!2))
             | otherwise      = error $ "vertex undefined for Vector of size=" ++ show (LA.size v)
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
text2D :: Float -> Float -> String -> IO ()
text2D x y s = do
    rasterPos (Vertex2 (floatGL x) (floatGL y))
    renderString Helvetica12 s

textAt :: Point -> String -> IO ()
textAt = textAtF Helvetica12

textAtF :: Font a => a -> Point -> String -> IO ()
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

clampfGL :: Float -> GLclampf
clampfGL = unsafeCoerce -- realToFrac

--------------------------------------------------------------------------------

data EVWindow st = EVW { evW        :: Window
                       , evSt       :: IORef st
                       , evReady    :: MVar Bool
                       , evDraw     :: MVar Drawing
                       , evAfterD   :: IORef (IO ())
                       , evNotify   :: IORef (IO ())
                       , evSync     :: IORef Bool
                       , evRegion   :: IORef WinRegion
                       , evDrReg    :: IORef Bool
                       , evMove     :: IORef MoveStatus
                       , evInit     :: IO ()
                       , evZoom     :: IORef (Double,Double,Double)
                       , evPrefSize :: IORef (Maybe Size)
                       , evPolicy   :: IORef ResizePolicy
                       , evVisible  :: IORef Bool
                       , evPause    :: IORef PauseStatus
                       , evStats    :: IORef WStatus
                       , evWinTitle :: String
                       , evEnd      :: IORef Bool
                       }

data MoveStatus = None | SetROI | MoveZoom GLint GLint

data ResizePolicy = UserSize | StaticSize | DynamicSize deriving Eq

data PauseStatus = NoPause | PauseCam | PauseDraw | PauseStep deriving Eq

type WinRegion = (Point,Point)

data WStatus = WStatus { evNDraw, evNCall :: Int }

--------------------------------------------------------------------------------


class Renderable x where
    render :: x -> IO ()
    renderIn :: EVWindow st -> x -> IO ()
    renderIn _ = render
    render = renderIn undefined

data Drawing = forall a . (Renderable a) => Draw a
             | Raw (IO())

instance Renderable Drawing where
    renderIn w (Draw x) = renderIn w x
    renderIn _ (Raw f) = f

instance Renderable [Drawing] where
    renderIn w = mapM_ (renderIn w)
    
instance Renderable a => Renderable (Maybe a) where
    renderIn w (Just x) = renderIn w x
    renderIn _ Nothing = return ()

draws :: Renderable a => [a] -> Drawing
draws = Draw . map Draw

--------------------------------------

instance Renderable (RGB Float) where
    render RGB {..} = currentColor $= Color4 (floatGL $ channelRed)
                                             (floatGL $ channelGreen)
                                             (floatGL $ channelBlue)
                                             1

clampfColor :: Colour Float -> Color4 GLclampf
clampfColor (toSRGB->RGB{..}) = Color4 (clampfGL $ channelRed)
                                       (clampfGL $ channelGreen)
                                       (clampfGL $ channelBlue)
                                       1

instance Renderable (Colour Float) where
    render = render . toSRGB 

color :: Renderable x => Colour Float -> x -> Drawing
color c d = Raw $ do
    c' <- get currentColor
    render c
    render d
    currentColor $= c'

lineWd :: Renderable x => Float -> x -> Drawing
lineWd w d = Raw $ do
    w' <- get lineWidth
    lineWidth $= floatGL w
    render d
    lineWidth $= w'

pointSz :: Renderable x => Float -> x -> Drawing
pointSz s d = Raw $ do
    s' <- get pointSize
    pointSize $= floatGL s
    render d
    pointSize $= s'

textF :: Font a => a -> Point -> String -> Drawing
textF f p s = Raw (textAtF f p s)

text :: Point -> String -> Drawing
text = textF Helvetica18

winTitle :: String -> Drawing
winTitle = Raw . (GL.windowTitle $=)

windowTitle :: Renderable a => String -> a -> Drawing
windowTitle name f = Draw [ winTitle name, Draw f ]

clearColor :: Renderable a => Colour Float -> a -> Drawing
clearColor c d = Draw [ Raw $ GL.clearColor $= clampfColor c, Draw d ]

instance Renderable () where
    render = return


withOrtho2D :: Renderable a => GLdouble -> GLdouble -> GLdouble -> GLdouble -> a -> Drawing
withOrtho2D x1 x2 y1 y2 f = Draw [g, Draw f]
  where g = Raw $ do
                matrixMode $= Projection
                loadIdentity
                ortho2D x1 x2 y1 y2
                matrixMode $= Modelview 0
                loadIdentity


colorAlpha :: Renderable x => Colour Float -> GLfloat -> x -> Drawing
colorAlpha c alpha d = color c $ Raw $ do
    GL.Color4 r g b a <- GL.get GL.currentColor
    GL.currentColor GL.$= GL.Color4 r g b alpha
    render d
    GL.currentColor GL.$= GL.Color4 r g b a


blend :: Renderable x => x -> Drawing
blend d = Raw $ do
    GL.blend GL.$= GL.Enabled
    GL.blendFunc   GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    render d
    GL.blend GL.$= GL.Disabled


setRegion :: WinRegion -> Image p -> Image p
setRegion (p1,p2) x = setROI (poly2roi (Image.size x) (Closed [p1,p2])) x

-----------------------------------------

prepZoom :: EVWindow st -> IO ()
prepZoom evW = do
    (Size h w) <- evSize `fmap` get windowSize
    (z,dx,dy) <- readIORef (evZoom evW)
    let zx = ( round (fromIntegral w*z) - w ) `div` 2
        zy = ( round (fromIntegral h*z) - h ) `div` 2
        sz = GL.Size (fromIntegral $ w+2*zx) (fromIntegral $ h+2*zy) 
    viewport $= (Position (-fromIntegral zx + round dx) (-fromIntegral zy + round dy), sz)
    pointCoords -- inates (evSize sz) 


unZoom :: (Double, Double, Double) -> (Position, GL.Size) -> (GLint, GLint) -> (Int, Int)
unZoom (z,dx,dy) (Position _vpx _vpy, sz) (x,y) = (round x', round y')
  where
    Size h w = evSize sz
    rh = fromIntegral h / z
    rw = fromIntegral w / z
    oh = (rh- fromIntegral h) / 2 + dy
    ow = (rw- fromIntegral w) / 2 + dx
    x' = (fromIntegral x-ow) / z
    y' = (fromIntegral y-oh) / z

