-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.ScatterPlot
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Show distribution of labeled vectors in space

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.ScatterPlot (
    scatterPlot, drawRegion,
    scatterPlot3D,
)where

import EasyVision.GUI
import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import Classifier
import Numeric.LinearAlgebra
import Data.Colour.Names as Col
import Control.Monad(forM_)
import ImagProc.Base(Point(..))


scatter examples (i,j) colornames prefun = do
    let (gs,lbs) = group examples
        plot = map (\v-> Point (v@>i) (v@>j))
        xs = map ((@>i).fst) examples
        ys = map ((@>j).fst) examples
        a1 = minimum xs
        a2 = maximum xs
        b1 = minimum ys
        b2 = maximum ys
        da = 0.05*(a2-a1)
        db = 0.05*(b2-b1)
        colors = take (length gs) $
                 map setColor' colornames
                 ++ [setColor 1 0 0, setColor 0 0 1, setColor 0 1 0] ++
                    [setColor 1 1 0, setColor 0 1 1, setColor 1 0 1] ++
                    [setColor 1 0.5 0.5, setColor 0.5 0.5 1, setColor 0.5 1 0.5]
                 ++ repeat (setColor 1 1 1)
    clear [ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    ortho2D (doubleGL $ a1-da) (doubleGL $ a2+da) (doubleGL $ b1-db) (doubleGL $ b2+db)
    matrixMode $= Modelview 0
    loadIdentity
    let f pts col = do
            pointSize $= 5
            setColor 0 0 0
            GL.renderPrimitive GL.Points . mapM_ GL.vertex . plot $ pts
            pointSize $= 3
            col
            GL.renderPrimitive GL.Points . mapM_ GL.vertex . plot $ pts

    prefun

    pointSize $= 3
    sequence_ $ zipWith f (reverse gs) (reverse colors)

    setColor 0.5 0.5 0.5
    textAt (Point a2 b1) (show i)
    textAt (Point a1 b2) (show j)


scatterPlot name sz exs coor colors prefun = do
    evWindow coor name sz (Just disp) kbd
    clearColor $= Color4 1 1 1 1
  where n = dim . fst . head $ exs
        disp rdesi = do
            coord <- get rdesi
            scatter exs coord colors prefun

        kbd rdesi (SpecialKey KeyUp) Down _ _ = do
            (i,j) <- getW rdesi
            putW rdesi $ (i,(j+1) `mod` n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyDown) Down _ _ = do
            (i,j) <- getW rdesi
            putW rdesi $ (i, (j-1) `mod`n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyRight) Down _ _ = do
            (i,j) <- getW rdesi
            putW rdesi $ ((i+1)`mod`n,j)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyLeft) Down _ _ = do
            (i,j) <- getW rdesi
            putW rdesi $ ((i-1) `mod` n,j)
            postRedisplay Nothing
        kbd _ a b c d = kbdQuit a b c d

drawRegion clasif prob colors = f where
    n = 71
    xs = map ((@>0).fst) prob
    ys = map ((@>1).fst) prob
    a1 = minimum xs
    a2 = maximum xs
    b1 = minimum ys
    b2 = maximum ys
    ranx = toList $ linspace n (a1,a2)
    rany = toList $ linspace n (b1,b2)
    dom = sequence [ranx,rany]
    themap = zip (labels . snd . group $  prob) (map setColor' colors)
    colorOf lab = maybe (setColor' Col.lightgray) id (lookup lab themap)
    vals = map (colorOf . clasif . fromList) dom `zip` dom
    f = do
        pointSize $= 2
        forM_ vals $ \(c,p) -> do
            c
            renderPrimitive Points (vertex p)

----------------------------------------------------------------------

defaultColors = [red, blue, green, orange, brown ] ++ repeat gray

scatter3D examples (i,j,k) colornames prefun = do
    let (gs,lbs) = group examples
        plot = mapM_ $ \v-> GL.vertex (GL.Vertex3 (v@>i) (v@>j) (v@>k))
        colors = take (length gs) $
                 map setColor' (colornames++defaultColors)
    let f pts col = do
            pointSize $= 3
            col
            GL.renderPrimitive GL.Points $ plot pts
    prefun

    pointSize $= 3
    sequence_ $ zipWith f ( gs) ( colors)
    setColor' gray
    GL.renderPrimitive GL.Lines $ mapM_ vertex [
        Vertex3 0 0 0,
        Vertex3 1 0 0,
        Vertex3 0 0 0,
        Vertex3 0 1 0,
        Vertex3 0 0 0,
        Vertex3 0 0 (1::GLfloat)]



scatterPlot3D name sz exs coor colors prefun = do
    w3D <- evWin3D coor name sz (Just disp) kbd
    clearColor $= Color4 1 1 1 1
  where n = dim . fst . head $ exs
        disp rdesi = do
            coord <- get rdesi
            scatter3D exs coord colors prefun
        kbd rdesi (SpecialKey KeyUp) Down _ _ = do
            (i,j,k) <- getW rdesi
            putW rdesi $ (i,(j+1) `mod` n,k)
        kbd rdesi (SpecialKey KeyDown) Down _ _ = do
            (i,j,k) <- getW rdesi
            putW rdesi $ (i, (j-1) `mod`n,k)
        kbd rdesi (SpecialKey KeyRight) Down _ _ = do
            (i,j,k) <- getW rdesi
            putW rdesi $ ((i+1)`mod`n,j,k)
        kbd rdesi (SpecialKey KeyLeft) Down _ _ = do
            (i,j,k) <- getW rdesi
            putW rdesi $ ((i-1) `mod` n,j,k)
        kbd _ a b c d = kbdQuit a b c d
