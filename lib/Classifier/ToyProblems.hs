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
-- * 2D toy problems
-- | You can take a look at them using 'combined'.
     linsep, linsepmulti, nosep, ring, moon, rings, sshape,
-- * MNIST database
     mnist, mnistraw,
-- * 2D drawing
     combined
) where

import Numeric.LinearAlgebra
import Data.List(intersperse)
--import qualified Data.Map as Map

import System
import Classifier.Base
import Classifier.Stat

------------------------- drawing utilities --------------------------

gnuplot command = do
    writeFile "gnuplotcommand" $ command
    system "gnuplot -persist gnuplotcommand" 
    system "rm gnuplotcommand"

prep = (++"e\n") . unlines . map (unwords . (map show))

{-
show2Dfun :: Int -> Double -> (Vector -> Double) -> IO ()
show2Dfun n r f = imshow (trans z) where
    l1 = toList $ linspace n (-r,r)
    l2 = reverse $ toList $ linspace n (-r,r)
    z = fromLists $ partit (length l1) $ [ f (fromList [x,y]) | x <- l1, y <- l2]
-}

{- | 2D representation of a 2D feature.

For instance:

@study :: Sample -\> Learner -\> IO ()
study prob meth = do
    seed \<- randomIO
    let (train,test) = splitProportion 0.5 $ scramble seed prob
    let (c,f) = meth train
    putStr \"Test error: \"
    print (errorRate test c)
    print (confusion test c)
    combined 100 2.5 (fromIntegral.posMax.f) train@

@\> study (nosep 500) (distance gaussian)@

@\> study (nosep 500) (multiclass mse)@

@\> study (rings 2000) (multiclass (adaboost 100 stumps))@

@\> study (addNoise 100 0.00001 (rings 2000))
          (multiclass (treeOf (branch 0) (unweight stumps)))@

-}
combined :: String -> Int -> Double -> (Vector Double ->Double) -> Sample -> IO ()
combined title n r f exs = act where
    (gs,_) = group exs
    l1 = toList $ linspace n (-r,r)
    l2 = toList $ linspace n (-r,r)
    z = [[x,y, f (fromList [x,y])] | x <- l1, y <- l2]
    z' = concat $ map (++[[]]) $ partit n z
    g m = toList m ++ [0]
    preps = concat (map p gs) where p gi = prep (map g gi)
    hs' = map ("\"-\" with points " ++) (map (show.code) [1 .. length gs])
    hs = concat (intersperse "," hs') ++ "\n"
    act = do
        gnuplot $ "set size square; set pm3d map explicit; set style data pm3d; set palette gray; "
                  ++ "set title " ++ show title ++ ";"
                  ++ "splot \"-\" with pm3d, "
                  ++ hs
                  ++ prep z'
                  ++ preps
        return ()
    code = ((38:57:83:18:30:[1..])!!)

---------------------------- some toy classification problems ---------------------------

-- | interlaced S-shapes
sshape :: Int -> Sample
sshape n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,4/5*2*pi)
    dat1 = [ (vector [0.5 -  1*cos t, 0.2 -1*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [-0.5 + 1*cos t, -0.2 + 1*sin t],   "b") | t <- ts m ]


-- | concentric semicircles
moon :: Int -> Sample
moon n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,pi)
    dat1 = [ (vector [2*cos t, 2*sin t -1],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t -1],         "b") | t <- ts m]

-- | 3 concentric rings (nonconvex solution)
rings :: Int -> Sample
rings n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vector [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t],         "b") | t <- ts (2*m)]
    dat3 = [ (vector [0.3*cos t, 0.3*sin t], "a") | t <- ts m ]

-- | concentric rings
ring :: Int -> Sample
ring n = dat1 ++ dat2 where
    m = n `quot` 2
    ts k = tail $ toList $ linspace (k+1) (0,2*pi)
    dat1 = [ (vector [2*cos t, 2*sin t],     "a") | t <- ts m ]
    dat2 = [ (vector [cos t, sin t],         "b") | t <- ts m]

--  | very simple problem
linsep :: Int -> Sample
linsep n = dat1 ++ dat2 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    dat1 = [ (vector [1+0.5*cos t, -1+0.8*sin t],   "a") | t <- ts ]
    dat2 = [ (vector [-1+cos t, 1-sin t], "b") | t <- ts ]

--  | simple multiclass problem 
linsepmulti :: Int -> Sample
linsepmulti n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 2
    ts = tail $ toList $ linspace (m+1) (0,2*pi)
    k = 0.7
    dat1 = [ (vector [1+k*cos t, -1+k*sin t],   "a") | t <- ts ]
    dat2 = [ (vector [-1+k*cos t, 1-k*sin t], "b") | t <- ts ]
    dat3 = [ (vector [1+k*cos t, 1-k*sin t], "c") | t <- ts ]

--  | simple nonlinearly separable problem 
nosep :: Int -> Sample
nosep n = dat1 ++ dat2 ++ dat3 where
    m = n `quot` 4
    ts m = tail $ toList $ linspace (m+1) (0,2*pi)
    k = 0.8
    dat1 = [ (vector [1+2*k*cos t, -1+k*sin t],   "a") | t <- ts m ]
    dat2 = [ (vector [-1+k*cos t, 1-2*k*sin t], "a") | t <- ts m ]
    dat3 = [ (vector [1+k*cos t, 1-k*sin t], "b") | t <- ts (2*m)]

-- | handwritten digits, partitioned and with desired number of pca dimensions
mnist :: Int -> Int -> IO (Sample, Sample)

mnist dim n = do
    m <- fromFile "../data/mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train, test) = splitAt n mnist

    let st = stat (fromRows $ map fst train)
    let f = encodeVector $ pca (NewDimension dim) st

    return (preprocess f train,
            preprocess f test)

-- | the mnist raw data
mnistraw :: Int -> IO (Sample,Sample)
mnistraw n = do
    m <- fromFile "../data/mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    return $ splitAt n mnist
