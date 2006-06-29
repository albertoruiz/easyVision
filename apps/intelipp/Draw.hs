module Draw where

import Graphics.UI.GLUT
import Ipp
import Data.IORef
import System.Exit
import Foreign (touchForeignPtr)
import Control.Monad(unless)


-- hopengl machinery copied from GSL.Drawing
           
dibujar f state = do
    st @ (State {frame = n}) <- readIORef state
    clear [ColorBuffer]
    unless (pause st) $ f n
    swapBuffers     
    
-----------------------------------------------------

data State = State { frame :: Int, pause :: Bool}

prepareOpenGL f sz = do 
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "OpenGL Image Raster"
    windowSize $= sz
    
    state <- newIORef (State {frame = 0, pause = False})
    displayCallback $= dibujar f state
    keyboardMouseCallback $= Just (keyboard state)
    idleCallback $= Just (modify state)
    --addTimerCallback 30 $ modify state
    
    {-
    createWindow "another window"
    windowSize $= sz
    displayCallback $= dibujar f state
    idleCallback $= Just (postRedisplay Nothing)
    -}
    
    mainLoop
            
modify state = do
    modifyIORef state $ \s -> if not (pause s) then s {frame = frame s +1} else s
    postRedisplay Nothing
    --addTimerCallback 30 $ modify state
    
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
    
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}

keyboard _ _ _ _ _ = return ()
 
---------------------------------------------------------------------

tp 1 = UnsignedByte
tp 4 = Float


drawPolyline xs = do 
    lineWidth $= 1
    currentColor $= Color4 1 0 0 1
    renderPrimitive LineStrip $ f xs where
        f [] = return ()
        f ([y,x]:r) = do
            vertex (Vertex2 (fromIntegral x::GLfloat) (fromIntegral y))
            f r
            
drawPolyline' _ = return ()

display m w h = do
    matrixMode $= Projection
    loadIdentity
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

imageShow' (w,h) f = do
    prepareOpenGL g (Size (fromIntegral w+2) (fromIntegral h+2)) where
    g k = do
        let m = f k
        display m w h
        
imageShow (w,h) f = do
    prepareOpenGL g (Size (fromIntegral w) (fromIntegral h)) where
    g k = do
        m <- f k
        display m w h
