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
-- Currently we use gnuplot and imageMagick; we should eventually use HOpenGL. 
-----------------------------------------------------------------------------

module GSL.Drawing(
    
    plot, parametricPlot, mplot, 
    
    splot, mesh, meshdom, 
    
    matrixToPGM, imshow
    
) where

import GSL.Base
import GSL.Derived
import GSL.Interface
--import GSL.Util
import Data.List(intersperse)
import System

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

> > let f x y = mmap cos (x |+| y) 
> > splot f (0,pi) (0,2*pi) 50    

-}
splot :: (M->M->M) -> (Double,Double) -> (Double,Double) -> Int -> IO () 
splot f rx ry n = mesh z where
    (x,y) = meshdom (linspace n rx) (linspace n ry)
    z = f x y

{- | given a list of vectors [x, y1, y2, ...] it draws y1, y2, ... etc. against x -}
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
    
    
{- | draws a list of functions over a desired range and with a desired number of points 

> > plot [vmap sin, vmap cos, vmap sin . ((2::Double) <>)] (0,2*pi) 1000

-}
plot :: [V->V] -> (Double,Double) -> Int -> IO ()
plot fs rx n = mplot (x: mapf fs x)
    where x = linspace n rx  

{- | Draws a parametric curve. For instance, to draw a spiral we can do something like:

> > parametricPlot (\t->(t .* vmap sin t,t .* vmap cos t)) (0,10*pi) 1000

-}    
parametricPlot :: (V->(V,V)) -> (Double, Double) -> Int -> IO ()
parametricPlot f rt n = mplot [fx, fy]
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
    minval = m // flatten // toList // maximum
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
