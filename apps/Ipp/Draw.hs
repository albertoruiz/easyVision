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
import Foreign (touchForeignPtr)
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
    pixelCoordinates (w,h)
    renderPrimitive LineLoop $ vertices'
       [[c1 r, r1 r],
        [c1 r, r2 r],
        [c2 r, r2 r],
        [c2 r, r1 r]]

--------------------------------------------------------------------------------

--avoid sizes which are not a power of 2
drawTexture im@Img {itype = I32f} [v1,v2,v3,v4] = do
    texImage2D  Nothing
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

  where
    vert :: TexCoord2 GLdouble -> [GLdouble] -> IO ()
    vert t [x,y,z] = do
          multiTexCoord (TextureUnit 0) t
          vertex (Vertex3 x y z)

------------------------------------------------------------

mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1

pointCoordinates (w,h) = draw2Dwith (ortho2D 1 (-1) (-r) r)
    where r = fromIntegral h / fromIntegral w

pixelCoordinates (w,h) = draw2Dwith (ortho2D 0 (fromIntegral w-1) (fromIntegral h-1) 0)

draw2Dwith ortho = do
    matrixMode $= Projection
    loadIdentity
    ortho
    matrixMode $= Modelview 0
    loadIdentity

vertices l = mapM_ (vertex.f) l where
    f [x,y]   = Vertex3 x y 0
    f [x,y,z] = Vertex3 x y z
    f _ = error "vertices without 2 or 3 components"

vertices' = vts . map (map fromIntegral) where
    vts :: [[Double]] -> IO ()
    vts = vertices

-- | It shows the outline of a camera and an optional image in it
showcam size cam Nothing = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (realVector[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    renderPrimitive LineLoop $ vertices outline

showcam size cam (Just imgtext) = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diag (realVector[1,1,1,1/size])
    let outline = ht m (cameraOutline f)
    let q = 1 --0.75                     TO DO: fix this
    drawTexture imgtext $ ht m
              [[ q,  q, f],
               [-q,  q, f],
               [-q, -q, f],
               [ q, -q, f]]
    renderPrimitive LineLoop $ vertices outline

drawCamera size cam im = showcam size cam (Just im)

extractSquare sz im = resize32f (sz,sz) im {vroi = roi} where
    w = width im
    h = height im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}
