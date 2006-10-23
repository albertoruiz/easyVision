{-# OPTIONS #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Drawing
-- Copyright   :  (c) Alberto Ruiz 2005
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  tested on GHC
--
-- Drawing utilities.
--
-----------------------------------------------------------------------------

module DrawingGL(

    meshOpenGL, plotOpenGL

) where

import GSL
import Data.List(intersperse)
import System
import Graphics.HGL hiding (Char, DoubleBuffered)
import Graphics.UI.GLUT hiding (Matrix, scale)
import qualified Graphics.UI.GLUT as G
import Data.IORef
import System.Exit
import Foreign hiding (rotate)


for = flip mapM_           
                      
           
drawMesh z@(M r c p) = do 
    renderPrimitive Triangles $ 
        for [1 .. r - 1] $ \i -> 
            for [1 .. c - 1] $ \j -> do
                vertex (Vertex3 (fromIntegral i) (fromIntegral j) (z i j))
                vertex (Vertex3 (fromIntegral i -1) (fromIntegral j) (z (i-1) j)) 
                vertex (Vertex3 (fromIntegral i) (fromIntegral j -1) (z i (j-1)))
 where z i j = unsafePerformIO $ withForeignPtr p $ \p -> peek (advancePtr p (i*c+j)) 

                 
{- | It draws an animated sequence of surfaces z = f k, where k is the frame number. For example:

@w k = fromIntegral k \/ 100 
rg = 'linspace' 40 (-2,2)
(x,y) = 'meshdom' rg rg
r2 = x*x+y*y
\ 
\> meshOpenGL $ \k -> exp (-r2) * cos (w k *r2)@

The inclination of the surface can be changed with KeyUp and KeyDown. The key P pauses the animation.

-}
meshOpenGL :: (Int -> Matrix Double) -> IO () 
meshOpenGL f = do
    prepareOpenGL g where
    g k = do
        
        let z = f k
        let r = fromIntegral (rows z)
        let c = fromIntegral (cols z)
        let sc = 1.8/fromIntegral (rows z) :: GLfloat
        --position (Light 0) $= Vertex4 2 2 (-3) 2
        --rotate (fromIntegral k) $ Vector3 0 1 (0::GLfloat)
        --rotate (fromIntegral k) $ Vector3 1 0 (0::GLfloat)
       
        rotate (fromIntegral k) $ Vector3 0 0 (1::GLfloat)
        position (Light 0) $= Vertex4 1 1 (-2) 1
        translate $ Vector3 (-0.9::GLfloat) (-0.9) 0
        G.scale sc sc (-1)        
        currentColor $= Color4 1 1 1 1
        polygonMode $= (Fill,Fill)
        meshC z
    
    
-----------------------------------------------------    
{- | It draws an animated sequence of polylines (x,y) = f k, where k is the frame number. For example:

>> let f k = 0.2 * fromIntegral k
>> plotOpenGL $ \k -> (x, cos x + 0.2 * cos (5*x- f k))

-}
plotOpenGL :: (Int -> (Vector Double, Vector Double)) -> IO ()
plotOpenGL f = do
    prepareOpenGL g where
    g k = do
        lineWidth $= 1
        currentColor $= Color4 1 1 1 1
        loadIdentity
        translate (Vector3 (-1) 0 (0::GLfloat))
        G.scale 0.5 0.5 (0.5::GLfloat)
        let (x,y) = f k
        drawPolyline x y


drawPolyline x y = do 
    renderPrimitive LineStrip $ mapM_ f [0 .. size x - 1] where
        f i = vertex (Vertex3 (x@>i) (y@>i) 0)
           


           
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

prepareOpenGL f = do 
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    createWindow "OpenGL Plot"
    windowSize $= Size 400 400
    lookAt (Vertex3 0 0 1) (Vertex3 0 0 0) (Vector3 1 0 0)
    lighting $= Enabled
    light (Light 0) $= Enabled
    
    depthFunc $= Just Less
    
    --polygonSmooth $= Enabled
    --lineSmooth $= Enabled
        
    state <- newIORef (State {frame = 0, pause = False, angle = 60})
    displayCallback $= dibujar f state
    keyboardMouseCallback $= Just (keyboard state)
    --idleCallback $= Just (newFrame frameNumber)
    addTimerCallback 30 $ modify state
    mainLoop
            
modify state = do
    modifyIORef state $ \s -> if not (pause s) then s {frame = frame s +1} else s
    postRedisplay Nothing
    addTimerCallback 30 $ modify state
    
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
    
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}

keyboard st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {angle = angle s + 5}

keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {angle = angle s - 5}

keyboard _ _ _ _ _ = return ()
