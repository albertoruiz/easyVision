{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
{- |
Module      :  Util.Rotation
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Rotation utilities.

-}
-----------------------------------------------------------------------------

module Util.Rotation
( rot1, rot1d, rot1g
, rot2, rot2d, rot2g
, rot3, rot3d, rot3g
) where

import Numeric.LinearAlgebra
import Util.Misc(Mat)



-- | 3x3 rotation around the X-axis
rot1 :: Double -> Mat
rot1 a = (3><3) [ 1, 0, 0
                , 0, c, s
                , 0,-s, c ]
    where c = cos a
          s = sin a

-- | 3x3 rotation around the Y-axis
rot2 :: Double -> Mat
rot2 a = (3><3) [ c, 0, s
                , 0, 1, 0
                ,-s, 0, c ]
    where c = cos a
          s = sin a

-- | 3x3 rotation around the Z-axis
rot3 :: Double -> Mat
rot3 a = (3><3) [ c, s, 0
                ,-s, c, 0
                , 0, 0, 1 ]
    where c = cos a
          s = sin a

-- | derivative of 'rot1'
rot1d :: Double -> Mat
rot1d a = (3><3) [0, 0,0,
                  0, c,s,
                  0,-s,c]
    where c = -sin a
          s =  cos a

-- | derivative of 'rot2'
rot2d :: Double -> Mat
rot2d a = (3><3) [ c,0,s,
                   0,0,0,
                  -s,0,c]
    where c = -sin a
          s =  cos a

-- | derivative of 'rot3'
rot3d :: Double -> Mat
rot3d a = (3><3) [ c,s,0,
                  -s,c,0,
                   0,0,0]
    where c = -sin a
          s =  cos a


-- | generator of 'rot1'
rot1g :: Mat
rot1g = (3><3) [0, 0,0
               ,0, 0,1
               ,0,-1,0]

-- | generator of 'rot2'
rot2g :: Mat
rot2g = (3><3) [ 0,0,1
               , 0,0,0
               ,-1,0,0]

-- | generator of 'rot3'
rot3g :: Mat
rot3g = (3><3) [ 0,1,0
               ,-1,0,0
               , 0,0,0]

