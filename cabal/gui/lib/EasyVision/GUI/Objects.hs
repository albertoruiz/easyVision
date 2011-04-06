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
    a4Ref, asymRef, cornerRef, houseModel, unitCube, sphere
) where

import Graphics.UI.GLUT
import EasyVision.GUI.Draw(setColor,setColor')
import Data.Colour.Names

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

v a b c = vertex $ Vertex3 a b (c::GLdouble)

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
unitCube d = do
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
    v a b c = vertex $ Vertex3 a b (c::GLdouble)
    v1 = v 0 0 0
    v2 = v d 0 0
    v3 = v d d 0
    v4 = v 0 d 0
    v5 = v 0 0 d
    v6 = v d 0 d
    v7 = v d d d
    v8 = v 0 d d

-- | draw sphere at x y z and radious r
sphere :: Double -> Double -> Double -> Double -> IO ()
sphere x y z r = do
    lineWidth $=1
    setColor 1 0.5 0.5
    translate $ Vector3 x y z
    renderQuadric 
        (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle)
        (Sphere (r*0.99) 20 20)
    setColor 0 0 0
    renderQuadric 
        (QuadricStyle Nothing NoTextureCoordinates Outside LineStyle)
        (Sphere r 20 20)
    translate $ Vector3 (-x) (-y) (-z)

