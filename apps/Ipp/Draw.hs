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

display m = do
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

genDrawTexture sz im = do
    dat <- getData im
    let rw = (width im - sz)  `quot` 2
    let rh = (height im - sz) `quot` 2
    let q s l = take sz $ drop s $ l
    let dat' = concat $ q rh $ map (q rw) $ dat
    let f [v1,v2, v3, v4] = do
        withArray dat' $ \p-> do
            texImage2D Nothing
                       NoProxy
                       0
                       Luminance'
                       (TextureSize2D 256 256)
                       0
                       (PixelData Luminance Float p)

        texture Texture2D $= Enabled
        renderPrimitive Polygon $ do
            vert (TexCoord2 0 0) v1
            vert (TexCoord2 1 0) v2
            vert (TexCoord2 1 1) v3
            vert (TexCoord2 0 1) v4
        texture Texture2D $= Disabled
     in return f


mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1


drawPixels xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))

drawPoints xs = do
    pointSize $= 3
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1) (1) (-0.75) (0.75)
    matrixMode $= Modelview 0
    loadIdentity
    renderPrimitive Points $ mapM_ (vertex.f) xs where
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
    drfun $ ht m
              [[ 1,  1, f],
               [-1,  1, f],
               [-1, -1, f],
               [ 1, -1, f]]
    renderPrimitive LineLoop $ mapM_ (vertex.d) outline

genDrawCamera size reso cam im = do
    drfun <- genDrawTexture reso im
    return $ showcam size cam (Just drfun)
