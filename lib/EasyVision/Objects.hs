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

module EasyVision.Objects (
    a4Ref, asymRef, cornerRef, houseModel
) where

import Graphics.UI.GLUT
import EasyVision.Draw(setColor)


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
