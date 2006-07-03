module Draw where

import Graphics.UI.GLUT
import Ipp
import Data.IORef
import Foreign (touchForeignPtr)


tp 1 = UnsignedByte
tp 4 = Float


display m= do
    matrixMode $= Projection
    loadIdentity
    let w = width m
    let h = height m
    ortho2D (0) (0.0001+fromIntegral w-1::GLdouble) (0) (0.0001+fromIntegral h-1)
    matrixMode $= Modelview 0
    loadIdentity
    
    rasterPos (Vertex2 (0::GLfloat) (fromIntegral h-1.0001))
    pixelZoom $= (1,-1)
    drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
                (PixelData Luminance (tp $ datasize m) (ptr m))
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
    renderPrimitive LineStrip $ f xs where
        f [] = return ()
        f ([y,x]:r) = do
            vertex (Vertex2 (fromIntegral x::GLfloat) (fromIntegral y))
            f r

  
installWindow name (wid,hei) imgtodraw kbdcallback state = do
    w <- createWindow name
    windowSize $= Size (fromIntegral wid) (fromIntegral hei)
    
    displayCallback $= draw
    keyboardMouseCallback $= Just (kbdcallback state)
    return w
  where
    draw = do
        clear [ColorBuffer]
        st <- readIORef state
        display (imgtodraw st)
        swapBuffers  
