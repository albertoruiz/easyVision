-----------------------------------------------------------------------------
{- |
Module      :  ScatterPlot
Copyright   :  (c) Alberto Ruiz 2007-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Show distribution of labeled vectors in space

-}
-----------------------------------------------------------------------------

module Vision.GUI.ScatterPlot (
    scatter, drawDecisionRegion,
    scatter3D
)where

import Vision.GUI.Types
import Vision.GUI.Draw
import Util.Geometry ( Point(..) )
import Numeric.LinearAlgebra hiding (i)
import Data.Colour ( Colour )
import Data.Colour.Names 
import Graphics.UI.GLUT as GL
    ( Vertex(vertex),
      Vertex3(Vertex3),
      PrimitiveMode(Points),
      renderPrimitive )
import qualified Data.List as L ( groupBy, group )
import Data.Function(on)
import Data.List(sort,sortBy)



type Example x = (x,String)
type Sample x = [Example x]

labels :: Sample x -> [String]
labels = map head . L.group . sort . map snd

group :: Sample x -> [[x]]
group = map (map fst) . L.groupBy ((==) `on` snd) . sortBy (compare `on` snd)

scatter :: Sample (Vector Double)
        -> (Int, Int) -> [Colour Float] -> Drawing -> Drawing
scatter examples (i,j) colornames prefun = clearColor white . prep $ [ prefun, pointSz 5 things]
  where
    gs = group examples
    things = zipWith f gs colors
    f g c = color c (plot g)
    plot = map (\v-> Point (v@>i) (v@>j))
    xs = map ((@>i).fst) examples
    ys = map ((@>j).fst) examples
    a1 = minimum xs
    a2 = maximum xs
    b1 = minimum ys
    b2 = maximum ys
    da = 0.05*(a2-a1)
    db = 0.05*(b2-b1)
    prep = withOrtho2D (doubleGL $ a1-da) (doubleGL $ a2+da) (doubleGL $ b1-db) (doubleGL $ b2+db)
    colors = take (length gs) (colornames ++ [red,blue,green,yellow,orange]++ repeat white)


drawDecisionRegion
  :: Int
     -> [Example (Vector Double)]
     -> [Colour Float]
     -> (Vector Double -> String)
     -> Drawing
drawDecisionRegion n prob colors clasif = pointSz 7 vals
  where
    xs = map ((@>0).fst) prob
    ys = map ((@>1).fst) prob
    a1 = minimum xs
    a2 = maximum xs
    b1 = minimum ys
    b2 = maximum ys
    ranx = toList $ linspace n (a1,a2)
    rany = toList $ linspace n (b1,b2)
    dom = sequence [ranx,rany]
    themap = zip (labels prob) (colors ++ [pink, lightblue, lightgreen, yellow, orange] ++ repeat white)
    colorOf lab = maybe white id (lookup lab themap)
    vals = map d dom
    d p = color (colorOf $ clasif $ fromList $ p) ((\[x,y]->Point x y) p)



scatter3D
  :: [Example (Vector Double)]
     -> (Int, Int, Int) -> [Colour Float] -> Drawing -> Drawing
scatter3D examples (i,j,k) colornames prefun = clearColor white $ [ prefun, pointSz 3 things, lineWd 1 . color black $ axes]
  where
    gs = group examples
    things = zipWith f gs colors
    f g c = color c (plot g)
    plot = Raw . GL.renderPrimitive GL.Points . mapM_ (\v-> vertex (Vertex3 (doubleGL $ v@>i) (doubleGL $ v@>j) (doubleGL $ v@>k))) -- FIXME using Point3D
    
    colors = take (length gs) (colornames++defaultColors)
    axes = axes3D 1
     
defaultColors :: [Colour Float]
defaultColors = [red, blue, green, orange, brown ] ++ repeat gray


