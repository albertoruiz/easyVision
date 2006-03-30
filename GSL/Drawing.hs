-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Drawing
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

module GSL.Drawing(
    
    hplot, plot, parametricPlot, mplot, 
    
    splot, mesh, meshdom, 
    
    matrixToPGM, imshow,
    
    meshOpenGL, plotOpenGL
    
) where

import GSL.Core
import GSL.Wrappers hiding (scale)
import GSL.Derived
import GSL.Interface
import Data.List(intersperse)
import System
import Graphics.HGL hiding (Char, DoubleBuffered)
import Graphics.UI.GLUT hiding (Matrix)
import Data.IORef
import System.Exit
import Foreign hiding (rotate)

------------------------------------------------------------------------

{- | Plots a list of vectors using the Haskell Graphics Library. Given a list of vectors [x, y1, y2, ...] it draws y1, y2, ... against x.

@> let x = 'linspace' 1000 (-2,2) 
hplot [x, exp(-x^2), 1+0.5 * cos (2*x)]@
 
-}
hplot :: [Vector] -> IO ()
hplot vs = windowHGL (prepPol vs) 

genPairs [] = []
genPairs [v] = [(r,v)] where r = realVector [1 .. fromIntegral (size v)]
genPairs [x,y] = [(x,y)]
genPairs (x:y:rs) = (x,y) : genPairs (x:rs)

getRanges pairs = ((mxx,mnx),(mxy,mny)) where
    (x,y) = unzip pairs
    mxx = maximum $ map (toScalar 4) x
    mnx = minimum $ map (toScalar 6) x
    mxy = maximum $ map (toScalar 4) y
    mny = minimum $ map (toScalar 6) y

adaptPair (rx,ry) (x,y) = zip (adaptVector (0,399) rx x) (adaptVector (299,0) ry y)

prepPol vs = map (polyline . adaptPair ranges) pairs where
    pairs = genPairs vs
    ranges = getRanges pairs
    
adaptVector:: (Int,Int) -> (Double, Double)-> Vector -> [Int]
adaptVector (a,b) (mx,mn) v = k <> v |+| c // toList // map round
    where vl = toList v
          k = if mx == mn then 0 else (fromIntegral b- fromIntegral a)/(mx-mn)
          c = fromIntegral a - k*mn

windowHGL grs = do 
    runGraphics $ withWindow_ "HGL Plot" (400, 300) $ \ w -> do
        drawInWindow w $ overGraphics grs  
        k<-getKey w
        return ()

-------------------------------------------------------------------------------


-- | From vectors x and y, it generates a pair of matrices to be used as x and y arguments for matrix functions.
meshdom :: Vector -> Vector -> (Matrix, Matrix)
meshdom r1 r2 = (outer r1 (constant 1 r2), outer (constant 1 r1) r2)
 

{- | Draws a 3D surface representation of a real matrix.

> > mesh (hilb 20)

In certain versions you can interactively rotate the graphic using the mouse.

-}
mesh :: Matrix -> IO ()
mesh m = do
    writeFile "splot-gnu-command" "splot \"splot-tmp.txt\" matrix with lines; pause -1"; 
    toFile "splot-tmp.txt" m
    putStr "Press [Return] to close the graphic and continue... "
    system "gnuplot splot-gnu-command"
    system "rm splot-tmp.txt splot-gnu-command"
    return ()

{- | Draws the surface represented by the function f in the desired ranges and number of points, internally using 'mesh'.

> > let f x y = cos (x + y) 
> > splot f (0,pi) (0,2*pi) 50    

-}
splot :: (Matrix->Matrix->Matrix) -> (Double,Double) -> (Double,Double) -> Int -> IO () 
splot f rx ry n = mesh z where
    (x,y) = meshdom (linspace n rx) (linspace n ry)
    z = f x y

{- | Similar to hplot, but using gnuplot -}
mplot :: [Vector] -> IO ()
mplot m = do
    writeFile "plot-gnu-command" (commands++endcmd)
    toFile "plot-tmp.txt" (fromCols m)
    putStr "Press [Return] to close the graphic and continue... "
    system "gnuplot plot-gnu-command"
    system "rm plot-tmp.txt plot-gnu-command"
    return ()
 where
    commands = if length m == 1 then command1 else commandmore
    command1 = "plot \"plot-tmp.txt\" with lines\n"
    commandmore = "plot " ++ plots ++ "\n"
    plots = concat $ intersperse ", " (map cmd [2 .. length m])
    cmd k = "\"plot-tmp.txt\" using 1:"++show k++" with lines"
    endcmd = "pause -1"


mapf :: [t -> a] -> t -> [a]
mapf [] _ = []
mapf (f:fs) a = f a: mapf fs a
    
    
{- | Draws a list of functions over a desired range and with a desired number of points 

> > plot [sin, cos, sin.(3*)] (0,2*pi) 1000

-}
plot :: [Vector->Vector] -> (Double,Double) -> Int -> IO ()
plot fs rx n = hplot (x: mapf fs x)
    where x = linspace n rx  

{- | Draws a parametric curve. For instance, to draw a spiral we can do something like:

> > parametricPlot (\t->(t * sin t, t * cos t)) (0,10*pi) 1000

-}    
parametricPlot :: (Vector->(Vector,Vector)) -> (Double, Double) -> Int -> IO ()
parametricPlot f rt n = hplot [fx, fy]
    where t = linspace n rt
          (fx,fy) = f t 
    
    
    
-- | writes a matrix to pgm image file
matrixToPGM :: String -> Matrix -> IO ()    
matrixToPGM filename m = do
    let ll = map (map f) (toLists m)
    writeFile filename $ header ++ unlines (map unwords ll)
 where 
    c = cols m
    r = rows m
    header = "P2 "++show c++" "++show r++" "++show (round maxgray :: Int)++"\n"
    maxgray = 255.0
    maxval = m // flatten // toList // maximum
    minval = m // flatten // toList // minimum
    scale = if (maxval == minval) 
        then 0.0
        else maxgray / (maxval - minval)
    f x = show ( round ( scale *(x - minval) ) :: Int )   
    
-- | imshow shows a representation of a matrix as a gray level image using ImageMagick's display and an auxilary text file
imshow :: Matrix -> IO ()
imshow m = do
    matrixToPGM "tmpimg.pgm" m
    putStr "close the image window to continue...\n"
    system "display -antialias -resize 300 tmpimg.pgm"
    system "rm tmpimg.pgm"
    return ()


-------------------------- Open GL ------------------------------------------

for = flip mapM_           
                      
           
drawMesh z@(M r c p) = do 
    renderPrimitive Triangles $ 
        for [1 .. r - 1] $ \i -> 
            for [1 .. c - 1] $ \j -> do
                vertex (Vertex3 (fromIntegral i) (fromIntegral j) (z i j))
                vertex (Vertex3 (fromIntegral i -1) (fromIntegral j) (z (i-1) j)) 
                vertex (Vertex3 (fromIntegral i) (fromIntegral j -1) (z i (j-1)))
 where z i j = unsafePerformIO $ withForeignPtr p $ \p -> peek (advancePtr p (i*c+j)) 

                 
{- | It draws an animated sequence of surfaces given by coordinates (x, y, z) = f k, where k is the frame number. For example:

@w k = fromIntegral k \/ 100 
rg = 'linspace' 40 (-2,2)
(x,y) = 'meshdom' rg rg
r2 = x*x+y*y
\ 
\> meshOpenGL $ \k -> exp (-r2) * cos (w k *r2)@

-}
meshOpenGL :: (Int -> Matrix) -> IO () 
meshOpenGL f = do
    prepareOpenGL g where
    g k = do
        pointSize $= 2
        currentColor $= Color4 1 0 0 0
        loadIdentity
        let z = f k
        let r = fromIntegral (rows z)
        let c = fromIntegral (cols z)
        let sc = 1.8/fromIntegral (rows z) :: GLfloat
        polygonMode $= (Line,Line)
        rotate (fromIntegral k) $ Vector3 0 1 (0::GLfloat)
        rotate (fromIntegral k) $ Vector3 1 0 (0::GLfloat)
        translate $ Vector3 (-0.9::GLfloat) (-0.9) 0
        scale sc sc 1
        --drawMesh z
        meshC z
    
    
-----------------------------------------------------    
{- | It draws an animated sequence of polylines (x,y) = f k, where k is the frame number. For example:

>> let f k = 0.2 * fromIntegral k
>> plotOpenGL $ \k -> (x, cos x + 0.2 * cos (5*x- f k))

-}
plotOpenGL :: (Int -> (Vector, Vector)) -> IO ()
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