-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Objects
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

True coordinates of several calibration objects and 3D models

-}
-----------------------------------------------------------------------------

module EasyVision.GUI.Objects (
    a4Ref, asymRef, cornerRef, houseModel, block, unitCube, sphere,
    module EasyVision.GUI.Model3DS
) where

import Graphics.UI.GLUT
import EasyVision.GUI.Util(doubleGL,setColor,setColor')
import Data.Colour.Names
import EasyVision.GUI.Model3DS

asymRef = map (map (*0.54))
       [ [ 0, 0]
       , [ 0, 2]
       , [ 1, 2]
       , [ 2, 1]
       , [ 2, 0] ]

cornerRef = map (map (*0.58))
       [ [ 0, 0]
       , [ 0, 2]
       , [ 1, 2]
       , [ 1, 1]
       , [ 2, 1]
       , [ 2, 0] ]


a4Ref = [[   0, 0]
        ,[   0, r]
        ,[   1, r]
        ,[   1, 0]] where r = sqrt 2

v a b c = vertex $ Vertex3 (doubleGL a) (doubleGL b) (doubleGL c)

houseModel = do
    setColor 1 0.5 0.5
    renderPrimitive Polygon $ do
        v 0.5 0 1.5
        v 1   0 1
        v 1   1 1
        v 0.5 1 1.5
        v 0.5 0 1.5
    setColor 1 0.3 0.3
    renderPrimitive Polygon $ do
        v 0   0   1
        v 0.5 0 1.5
        v 0.5 1 1.5
        v 0   1   1
        v 0   0   1
    setColor 1 0.6 1
    renderPrimitive Polygon $ do
        v 0 0 0
        v 0 0 1
        v 0 1 1
        v 0 1 0
        v 0 0 0
    setColor 0.6 1 1
    renderPrimitive Polygon $ do
        v 1 0 0
        v 1 0 1
        v 1 1 1
        v 1 1 0
        v 1 0 0

-- | cube of give size at the origin
unitCube :: Double -> IO ()
unitCube d = block d d d

block :: Double -> Double -> Double -> IO ()
block x y z = do
    setColor' red
    renderPrimitive Polygon $ v5 >> v6 >> v7 >> v8
    setColor' green
    renderPrimitive Polygon $ v1 >> v2 >> v6 >> v5
    setColor' blue
    renderPrimitive Polygon $ v1 >> v4 >> v8 >> v5
    setColor' yellow
    renderPrimitive Polygon $ v2 >> v3 >> v7 >> v6
    setColor' orange
    renderPrimitive Polygon $ v3 >> v4 >> v8 >> v7
    setColor' purple
    renderPrimitive Polygon $ v1 >> v2 >> v3 >> v4
  where
    v1 = v 0 0 0
    v2 = v x 0 0
    v3 = v x y 0
    v4 = v 0 y 0
    v5 = v 0 0 z
    v6 = v x 0 z
    v7 = v x y z
    v8 = v 0 y z


-- | draw sphere at x y z and radious r
sphere :: Double -> Double -> Double -> Double -> IO ()
sphere x y z r = do
    lineWidth $=1
    setColor 1 0.5 0.5
    preservingMatrix $ do
        translate $ Vector3 (doubleGL x) (doubleGL y) (doubleGL z)
        renderQuadric 
            (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
            (Sphere (doubleGL $ r*0.99) 20 20)
        setColor 0 0 0
        renderQuadric 
            (QuadricStyle Nothing NoTextureCoordinates Outside LineStyle)
            (Sphere (doubleGL r) 20 20)

