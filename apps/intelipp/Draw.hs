module Draw where

import Graphics.UI.GLUT hiding (RGB)
import qualified Graphics.UI.GLUT as GL
import Ipp
import Data.IORef
import Foreign (touchForeignPtr)

myDrawPixels m@Img{itype=RGB} = 
    drawPixels (Size (fromIntegral $ step m `quot` 3) (fromIntegral $ height m))
               (PixelData GL.RGB UnsignedByte (ptr m))

myDrawPixels m@Img{itype=Gray} = 
    drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
               (PixelData Luminance UnsignedByte (ptr m))

myDrawPixels m@Img{itype=I32f} = 
    drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
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

installWindow name (wid,hei) (Just fun) kbdcallback state = do
    w <- createWindow name
    windowSize $= Size (fromIntegral wid) (fromIntegral hei)
    
    displayCallback $= draw
    keyboardMouseCallback $= Just (kbdcallback state)
    return w
  where
    draw = do
        clear [ColorBuffer]
        st <- readIORef state
        fun st
        swapBuffers  

installWindow name (wid,hei) Nothing kbdcallback state = do
    w <- createWindow name
    windowSize $= Size (fromIntegral wid) (fromIntegral hei)
    keyboardMouseCallback $= Just (kbdcallback state)
    return w 