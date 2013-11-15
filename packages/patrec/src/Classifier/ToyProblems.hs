-----------------------------------------------------------------------------
{- |
Module      :  Classifier.ToyProblems
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some 2D toy problems useful for testing classifiers.

-}
-----------------------------------------------------------------------------

module Classifier.ToyProblems (
     linsep, linsepmulti, nosep, ring, moon, rings, sshape, testProb
) where

import Numeric.LinearAlgebra
import Classifier.Base
import Util.Misc(vec,Vec)


-- | interlaced S-shapes
sshape :: Int -> Sample Vec
sshape n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,4/5*2*pi)
    dat1 = [ (vec [0.5 -  1*cos t, 0.2 -1*sin t],     "a") | t <- ts m ]
    dat2 = [ (vec [-0.5 + 1*cos t, -0.2 + 1*sin t],   "b") | t <- ts m ]


-- | concentric semicircles
moon :: Int -> Sample Vec
moon n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,pi)
    dat1 = [ (vec [2*cos t, 2*sin t -1],     "a") | t <- ts m ]
    dat2 = [ (vec [cos t, sin t -1],         "b") | t <- ts m]

-- | 3 concentric rings (nonconvex solution)
rings :: Int -> Sample Vec
rings n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vec [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vec [cos t, sin t],         "b") | t <- ts (2*m)]
    dat3 = [ (vec [0.3*cos t, 0.3*sin t], "a") | t <- ts m ]

-- | concentric rings
ring :: Int -> Sample Vec
ring n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vec [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vec [cos t, sin t],         "b") | t <- ts m]

--  | very simple problem
linsep :: Int -> Sample Vec
linsep n = dat1 ++ dat2 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    dat1 = [ (vec [1+0.5*cos t, -1+0.8*sin t],   "a") | t <- ts ]
    dat2 = [ (vec [-1+cos t, 1-sin t], "b") | t <- ts ]

--  | simple multiclass problem
linsepmulti :: Int -> Sample Vec
linsepmulti n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    k = 0.7
    dat1 = [ (vec [1+k*cos t, -1+k*sin t],   "a") | t <- ts ]
    dat2 = [ (vec [-1+k*cos t, 1-k*sin t], "b") | t <- ts ]
    dat3 = [ (vec [1+k*cos t, 1-k*sin t], "c") | t <- ts ]

--  | simple nonlinearly separable problem 
nosep :: Int -> Sample Vec
nosep n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts r = tail $ toList $ linspace (r+1) (0,2*pi)
    k = 0.8
    dat1 = [ (vec [1+2*k*cos t, -1+k*sin t],   "a") | t <- ts m ]
    dat2 = [ (vec [-1+k*cos t, 1-2*k*sin t], "a") | t <- ts m ]
    dat3 = [ (vec [1+k*cos t, 1-k*sin t], "b") | t <- ts (2*m)]


testProb :: Sample (Vec)
testProb = [ (v[1,1],"1")
           , (v[2,1],"1")
           , (v[1,2],"1")
           , (v[2,2],"1")
           , (v[4,4],"2")
           , (v[5,4],"2")
           , (v[4,5],"2")
           , (v[5,5],"2")]
    where v = vec
