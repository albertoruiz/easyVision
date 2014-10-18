import Vision.GUI.Simple
import Util.Geometry
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(rand,randn)
import System.Random(randomIO)
import Util.Misc(vec,degree)
import Util.Statistics(randomPermutation)
import Util.Homogeneous(homog,inHomog)
import Vision(cross,mS)
import Util.Estimation(ransac)
import Util.Ellipses(circle)
import Contours(bisector)
import qualified Util.Geometry as G
import Control.Applicative((<$>))

--------------------------------------------------------------------------------

main = runIt $ procCircle >> procLine

--------------------------------------------------------------------------------

gpoints meth sigma m n = do
    inliers  <- toLists <$> noisy sigma (meth m)
    outliers <- toLists . (\r -> 2*r-1) <$> rand n 2
    seed <- randomIO
    return (map l2p $ randomPermutation seed (inliers ++ outliers))
  where
    noisy sigma m = (\r -> m + scalar sigma * r) <$> randn (rows m) (cols m)

l2p [x,y] = Point x y
p2l (Point x y) = [x,y]

----------------------------------

gpointsLine = gpoints $ \m -> linspace m (-0.5,0.5) `outer` fromList[1,1] 

gpointsCircle = gpoints $ \m -> fromLists $ map p2l $ circle m 0.5 (Point 0 0)

--------------------------------------------------------------------------------

estimateLine = ransac estimator inlier 2 0.99
  where
    estimator [a,b] = gjoin a b
    inlier l p = distE2 l p < 0.01**2

distE2 (HLine l1 l2 l3) (Point x y) = (l1*x + l2*y + l3)**2/(l1**2 + l2**2)


estimateCircle = ransac estimator inlier 3 0.99
  where
    estimator [a,b,c] = (cent,r)
      where cent = G.inhomog $ bisector (Segment a b) `G.meet` bisector (Segment b c)
            r = distPoints cent a
    inlier (cent,r) p = abs (distPoints cent p - r) < 0.02

--------------------------------------------------------------------------------

genproc name genpts estimator shmod = do
    pts <- genpts
    let okmodel = estimator pts
    print $ fst okmodel
    browser name (sh pts okmodel) (const id)
 where
    sh pts mod = [ clearColor white dat
                 , clearColor white [inls,drmodel] ]
      where
        dat = color black . pointSz 3 $ pts
        drmodel = color black (shmod model)
        inls = color black . pointSz 3 $ inliers
        (model,inliers) = mod

----------------------------------

procLine = genproc "line" (gpointsLine 0.02 50 500) estimateLine id

procCircle = genproc "circle" (gpointsCircle 0.02 100 500) estimateCircle sh
  where
    sh (cent,r) = Closed $ circle 50 r cent

