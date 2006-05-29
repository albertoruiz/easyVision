module Draw where

import Graphics.UI.GLUT
import Ipp
import Data.IORef
import System.Exit
import Foreign (touchForeignPtr)


-- hopengl machinery copied from GSL.Drawing, must be cleaned up
           
dibujar f state = do
    st @ (State {frame = n}) <- readIORef state
    --clearColor $= Color4 0 0 0 0.1
    clear [ColorBuffer,DepthBuffer]
    loadIdentity
    rotate (angle st) $ Vector3 1 0 (0::GLfloat)
    f n
    swapBuffers     
    
-----------------------------------------------------

data State = State { frame :: Int, pause :: Bool, angle :: GLfloat}

prepareOpenGL f sz = do 
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    createWindow "OpenGL Image Raster"
    windowSize $= sz
    lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 1 0 0)
    lighting $= Enabled
    light (Light 0) $= Enabled
    
    depthFunc $= Just Less
    
    --polygonSmooth $= Enabled
    --lineSmooth $= Enabled
        
    state <- newIORef (State {frame = 0, pause = False, angle = 60})
    displayCallback $= dibujar f state
    keyboardMouseCallback $= Just (keyboard state)
    idleCallback $= Just (modify state)
    --addTimerCallback 30 $ modify state
    mainLoop
            
modify state = do
    modifyIORef state $ \s -> if not (pause s) then s {frame = frame s +1} else s
    postRedisplay Nothing
    --addTimerCallback 30 $ modify state
    
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
    
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}

keyboard st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {angle = angle s + 5}

keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {angle = angle s - 5}

keyboard _ _ _ _ _ = return ()
 
---------------------------------------------------------------------

imageShow (w,h) f = do
    prepareOpenGL g (Size w h) where
    g k = do
        let m = f k
        drawPixels (Size (fromIntegral $ step m `quot` 4) (fromIntegral $ rows m))
                   (PixelData Luminance Float (ptr m))
        touchForeignPtr (fptr m)
        
imageShow' (w,h) f = do
    prepareOpenGL g (Size w h) where
    g k = do
        m <- f k
        drawPixels (Size (fromIntegral $ step m `quot` 4) (fromIntegral $ rows m))
                   (PixelData Luminance Float (ptr m))
        touchForeignPtr (fptr m)
        