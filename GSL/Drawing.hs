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
    
    matrixToPGM, imshow
    
) where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import Data.List(intersperse)
import System
import Graphics.HGL

------------------------------------------------------------------------

{- | Plots a list of vectors using the Haskell Graphics Library. Given a list of vectors [x, y1, y2, ...] it draws y1, y2, ... against x.

>> let x = linspace 1000 (-2,2) 
>hplot [x, exp(-x^2), 1+0.5 * cos (2*x)]
 
-}
hplot :: [V] -> IO ()
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
    
adaptVector:: (Int,Int) -> (Double, Double)-> V -> [Int]
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


-- | From vectors x and y, it generates a pair of matrices to be used as x and y arguments for matrix functions. It is used by 'splot' to build a 'mesh'.
meshdom :: V -> V -> (M, M)
meshdom r1 r2 = (outer r1 (constant 1 r2), outer (constant 1 r1) r2)
 

{- | Draws a 3D surface representation of a real matrix.

> > mesh (hilb 20)

In certain versions you can interactively rotate the graphic using the mouse.

-}
mesh :: M -> IO ()
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
splot :: (M->M->M) -> (Double,Double) -> (Double,Double) -> Int -> IO () 
splot f rx ry n = mesh z where
    (x,y) = meshdom (linspace n rx) (linspace n ry)
    z = f x y

{- | Similar to hplot, but using gnuplot -}
mplot :: [V] -> IO ()
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
plot :: [V->V] -> (Double,Double) -> Int -> IO ()
plot fs rx n = hplot (x: mapf fs x)
    where x = linspace n rx  

{- | Draws a parametric curve. For instance, to draw a spiral we can do something like:

> > parametricPlot (\t->(t * sin t, t * cos t)) (0,10*pi) 1000

-}    
parametricPlot :: (V->(V,V)) -> (Double, Double) -> Int -> IO ()
parametricPlot f rt n = hplot [fx, fy]
    where t = linspace n rt
          (fx,fy) = f t 
    
    
    
-- | writes a matrix to pgm image file
matrixToPGM :: String -> M -> IO ()    
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
imshow :: M -> IO ()
imshow m = do
    matrixToPGM "tmpimg.pgm" m
    putStr "close the image window to continue...\n"
    system "display -antialias -resize 300 tmpimg.pgm"
    system "rm tmpimg.pgm"
    return ()
