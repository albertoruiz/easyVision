{-# LANGUAGE FlexibleInstances, ViewPatterns #-}

import EasyVision
import Graphics.UI.GLUT as GL hiding(Point,Size) 
import Numeric.LinearAlgebra
import System.Random(randomIO)
import Util.Misc(randomPermutation,vec,debug,degree)
import Util.Homogeneous(homog,inHomog)
import Vision(cross,mS)
import Util.Estimation(ransac)
import Util.Ellipses(conicPoints,InfoEllipse(..))

----------------------------------------------------------------------

main = prepare >> procCircle >> mainLoop

----------------------------------------------------------------------

sz = (Size 400 400) 

gpoints sigma m n = do
    seed <- randomIO
    let --inliers = toLists $ noisy seed sigma ( linspace m (-0.5,0.5) `outer` fromList[1,1] )
        inliers = toLists $ noisy seed sigma (circle m)
        outliers = toLists $ reshape 2 $ 2 * randomVector seed Uniform (2*n) - 1
    return (randomPermutation seed (inliers ++ outliers))

noisy seed sigma m = m + scalar sigma * reshape (cols m) (randomVector seed Gaussian (rows m*cols m))

circlePoints n (x,y,r) = conicPoints n InfoEllipse {conicCenter = (x,y), conicSizes = (r,r), conicAngle = 0, conicTrans = undefined, conicMatrix = undefined}

circle m = fromLists $ map p2l $ circlePoints m (0,0,0.5)
  where p2l (Point x y) = [x,y]

estimateLine = ransac estimator inlier 2 0.99
  where
    estimator [a,b] = toList $ hv a `cross` hv b
    inlier l p = distE2 l p < 0.01**2

hv = homog.vec

distE2 [l1,l2,l3] [x,y] = (l1*x + l2*y + l3)**2/(l1**2 + l2**2)

mediatrix a' b' = d `cross` c
  where
    a = homog a'
    b = homog b'
    c = homog $ (a'+b')/2
    d = mS <> (a `cross` b)


estimateCircle = ransac estimator inlier 3 0.99
  where
    estimator [a@[ax,ay],b,c] = [x,y,r]
      where [x,y] = toList $ inHomog $ mediatrix (vec a) (vec b) `cross` mediatrix (vec b) (vec c)
            r = distPoints (Point x y) (Point ax ay)
    inlier [x,y,r] [p,q] = abs (distPoints (Point x y) (Point p q) - r) < 0.02


procLine = do
    pts <- gpoints 0.02 50 500
    let okmodel = estimateLine pts
    evWindow () "Data" sz (Just $ disp0 (pts)) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
    evWindow () "Model" sz (Just $ disp shLine (pts,okmodel)) (const kbdQuit)
    clearColor $= Color4 1 1 1 1

procCircle = do
    pts <- gpoints 0.02 100 500
    let okmodel = estimateCircle pts
    print $ fst okmodel
    evWindow () "Data" sz (Just $ disp0 (pts)) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
    evWindow () "Model" sz (Just $ disp shCircle (pts,okmodel)) (const kbdQuit)
    clearColor $= Color4 1 1 1 1


main' = do
    pts <- gpoints 0.02 50 500
    print $ fst $ estimateLine pts

----------------------------------------------------------------------

disp f (pts,(l,inl)) _ = do
    pointCoordinates sz
    pointSize $= 3
    setColor 0 0 0
    renderPrimitive Points $ mapM_ vertex pts
    setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex inl
    f l

disp0 pts _ = do
    pointCoordinates sz
    pointSize $= 3
    setColor 0 0 0
    renderPrimitive Points $ mapM_ vertex pts

shCircle [x,y,r] = renderPrimitive LineLoop $ vertex $ Closed $ circlePoints 35 (x,y,r)

