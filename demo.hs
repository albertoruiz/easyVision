module Main where

import GSL
import Graphics.UI.GLUT
import Data.IORef
import GSL.Core((!:),(!!:))
import System.Exit

x = linspace 1000 (0,2*pi)

main = do
    -- hplot [x, sin x + 0.2 * sin (10*x)]
    -- let f k = 0.2 * fromIntegral k
    -- plotOpenGL $ \k -> (x, cos x + 0.2 * cos (5*x- f k))
    let f k = 1.0 * fromIntegral k
    let rg = linspace 50 (-2,2)
    let (x,y) = meshdom rg rg
    let r2 = x*x+y*y
    meshOpenGL $ \k -> (x,y,exp (-r2) * cos ((fromIntegral k/100 ::Double) <>r2))
                    
-----------------------------------------------------    
    
meshOpenGL f = do
    prepareOpenGL g where
    g k = do
        pointSize $= 2
        currentColor $= Color4 1 0 0 0
        loadIdentity
        let (x,y,z) = f k
        --let r = fromIntegral (rows z)
        --let c = fromIntegral (cols z)
        --let sc = 1/(fromIntegral 0 +1)/r :: GLfloat
        -- scale sc sc 0.5
        -- translate $ Vector3 (-r/2::GLfloat) (-r/2) 0
        
        scale (0.5::GLfloat) 0.5 0.5
        rotate (fromIntegral k) $ Vector3 0 1 (0::GLfloat)
        rotate (fromIntegral k) $ Vector3 1 0 (0::GLfloat)
        
        drawMesh x y z          
    
-----------------------------------------------------    
plotOpenGL f = do
    prepareOpenGL g where
    g k = do
        lineWidth $= 1
        currentColor $= Color4 1 1 1 1
        loadIdentity
        translate (Vector3 (-1) 0 (0::GLfloat))
        scale 0.5 0.5 (0.5::GLfloat)
        let (x,y) = f k
        drawPolyline x y


drawPolyline x y = do 
    renderPrimitive LineStrip $ mapM_ f [0 .. size x - 1] where
        f i = vertex (Vertex3 (x!:i) (y!:i) 0)
           
for = flip mapM_           
           
drawMesh x y z = do 
    renderPrimitive Points $ 
        for [0 .. rows z - 1] $ \i -> 
            for [0 .. cols z - 1] $ \j -> do
                vertex (Vertex3 (x!!:(i,j)) (y!!:(i,j)) (z!!:(i,j)))
                --vertex (Vertex3 (fromIntegral j) (fromIntegral i) (z!!:(i,j)))
                --print (i,j,x!!:(i,j))
             


           
dibujar f frameNumber = do
    n <- readIORef frameNumber
    --clearColor $= Color4 1 0 0 1
    clear [ColorBuffer]
    f n
    swapBuffers     
    
-----------------------------------------------------

prepareOpenGL f = do 
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    createWindow "OpenGL Plot"
    windowSize $= Size 400 400
    keyboardMouseCallback $= Just keyboard

    --lighting $= Enabled
    position (Light 0) $= Vertex4 0 1 (-0.3) 0.1
    light (Light 0) $= Enabled
    
    --depthFunc $= Just Less
    
    polygonSmooth $= Enabled
    lineSmooth $= Enabled
        
    frameNumber <- newIORef (0::Int) -- the state is just the frame number
    displayCallback $= dibujar f frameNumber
    --idleCallback $= Just (newFrame frameNumber)
    addTimerCallback 2000 $ newFrame frameNumber
    mainLoop

oper fun var = modifyIORef var fun
            
newFrame state = do
    oper (+1) state
    postRedisplay Nothing
    addTimerCallback 30 $ newFrame state
    
keyboard (Char '\27') _ _ _ = do
    exitWith ExitSuccess

keyboard _ _ _ _ = return ()