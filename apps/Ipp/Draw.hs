-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Draw
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

HOpenGL drawing utilities.

-}
-----------------------------------------------------------------------------

module Ipp.Draw where

import Graphics.UI.GLUT hiding (RGB)
import qualified Graphics.UI.GLUT as GL
import Ipp.Core
import Ipp.Typical(resize32f)
import Data.IORef
import Foreign (touchForeignPtr, withArray)
import GSL
import Vision

myDrawPixels m@Img{itype=RGB} = 
    GL.drawPixels (Size (fromIntegral $ step m `quot` 3) (fromIntegral $ height m))
                  (PixelData GL.RGB UnsignedByte (ptr m))

myDrawPixels m@Img{itype=Gray} = 
    GL.drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
                  (PixelData Luminance UnsignedByte (ptr m))

myDrawPixels m@Img{itype=I32f} = 
    GL.drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
                  (PixelData Luminance Float (ptr m))

drawImage m = do
    matrixMode $= Projection
    loadIdentity
    let w = width m
    let h = height m
    ortho2D (0) (0.0001+fromIntegral w-1::GLdouble) (0) (0.0001+fromIntegral h-1)
    matrixMode $= Modelview 0
    loadIdentity
    rasterPos (Vertex2 (0::GLfloat) (fromIntegral h-1.0001))
    pixelZoom $= (1,-1)
    myDrawPixels m
    touchForeignPtr (fptr m)
    let r = shrink (-1,-1) $ vroi m
    drawPolyline [[h-1 - r1 r,c1 r],
                  [h-1 - r2 r,c1 r],
                  [h-1 - r2 r,c2 r],
                  [h-1 - r1 r,c2 r],
                  [h-1 - r1 r,c1 r]]

drawPolyline xs = do 
    lineWidth $= 1
    currentColor $= Color4 1 0 0 1
    renderPrimitive LineStrip $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (fromIntegral y))

--------------------------------------------------------------------------------

vert :: TexCoord2 GLdouble -> [GLdouble] -> IO ()
vert t [x,y,z] = do
          multiTexCoord (TextureUnit 0) t
          vertex (Vertex3 x y z)


--avoid sizes which are not a power of 2
drawTexture im@Img {itype = I32f} [v1,v2,v3,v4] = do
    {-# SCC "texImage2D" #-} texImage2D Nothing
                NoProxy
                0
                Luminance'
                (TextureSize2D (fromIntegral $ width im) (fromIntegral $ height im))
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

mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1


-- TO DO: rewrite the following functions:

drawPixels xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))

drawPoints xs = do
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1) (1) (-0.75) (0.75)
    matrixMode $= Modelview 0
    loadIdentity
    renderPrimitive Points $ mapM_ (vertex.f) xs where
        f [x,y] = Vertex2 (-x) y

drawPolygon xs = do
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1) (1) (-0.75) (0.75)
    matrixMode $= Modelview 0
    loadIdentity
    renderPrimitive LineLoop $ mapM_ (vertex.f) xs where
        f [x,y] = Vertex2 (-x) y


-- | It shows the outline of a camera and an optional image in it
showcam size cam Nothing = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (realVector[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    let d [x,y,z] = Vertex3 x y z
    renderPrimitive LineLoop $ mapM_ (vertex.d) outline

showcam size cam (Just drfun) = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (realVector[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    let d [x,y,z] = Vertex3 x y z
    let q = 1 --0.75                     TO DO: fix this
    drfun $ ht m
              [[ q,  q, f],
               [-q,  q, f],
               [-q, -q, f],
               [ q, -q, f]]
    renderPrimitive LineLoop $ mapM_ (vertex.d) outline

drawCamera size cam im = showcam size cam (Just (drawTexture im))

extractSquare sz im = resize32f (sz,sz) im {vroi = roi} where
    w = width im
    h = height im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}
