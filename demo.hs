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
    let rg = linspace 40 (-2,2)
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
        polygonMode $= (Line,Line)
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
    renderPrimitive Triangles $ 
        for [1 .. rows z - 1] $ \i -> 
            for [1 .. cols z - 1] $ \j -> do
                vertex (Vertex3 (x!!:(i,j)) (y!!:(i,j)) (z!!:(i,j)))
                vertex (Vertex3 (x!!:(i-1,j)) (y!!:(i-1,j)) (z!!:(i-1,j)))
                vertex (Vertex3 (x!!:(i,j-1)) (y!!:(i,j-1)) (z!!:(i,j-1)))
                --vertex (Vertex3 (fromIntegral j) (fromIntegral i) (z!!:(i,j)))
             


           
dibujar f state = do
    (n,_) <- readIORef state
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
    

    --lighting $= Enabled
    position (Light 0) $= Vertex4 0 1 (-0.3) 0.1
    light (Light 0) $= Enabled
    
    --depthFunc $= Just Less
    
    --polygonSmooth $= Enabled
    --lineSmooth $= Enabled
        
    state <- newIORef (0::Int, True) -- the state is the frame number and the pausing mode
    displayCallback $= dibujar f state
    keyboardMouseCallback $= Just (keyboard state)
    --idleCallback $= Just (newFrame frameNumber)
    addTimerCallback 2000 $ modify state
    mainLoop
            
modify state = do
    modifyIORef state $ \(n,s) -> if s then (n+1,s) else (n,s)
    postRedisplay Nothing
    addTimerCallback 30 $ modify state
    
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
    
keyboard st (Char 'p') Up _ _ = do
    modifyIORef st $ \(n,s) -> (n,not s)

keyboard _ _ _ _ _ = return ()