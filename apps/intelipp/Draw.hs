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


display m w h = do
    matrixMode $= Projection
    loadIdentity
    ortho2D 0.0 (fromIntegral w ::GLdouble) 0.0 (fromIntegral h)
    matrixMode $= Modelview 0
    loadIdentity
    
    rasterPos (Vertex2 (1::GLfloat) (fromIntegral h-1))
    pixelZoom $= (1,-1)
    drawPixels (Size (fromIntegral $ step m `quot` (datasize m)) (fromIntegral $ height m))
                (PixelData Luminance (tp $ datasize m) (ptr m))
    touchForeignPtr (fptr m)

imageShow' (w,h) f = do
    prepareOpenGL g (Size (fromIntegral w+2) (fromIntegral h+2)) where
    g k = do
        let m = f k
        display m w h
        
imageShow (w,h) f = do
    prepareOpenGL g (Size (fromIntegral w+2) (fromIntegral h+2)) where
    g k = do
        m <- f k
        display m w h
