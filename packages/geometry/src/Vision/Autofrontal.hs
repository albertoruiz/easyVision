-----------------------------------------------------------------------------
{- |
Module      :  Vision.Autofrontal
Copyright   :  (c) Alberto Ruiz 2006
License     :  GP

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Practical Planar Metric Rectification.
Ruiz et al. BMVC06

-}
-----------------------------------------------------------------------------

module Vision.Autofrontal (
    KnownFs(..), PolarHorizon, MetricInfo,
    consistency,
    camera0,
    findSol,
    autoOrthogonality,
    extractInfo
) where

-- experiments on planar rectification

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(unitary,norm)
import Numeric.GSL.Minimization
import Util.Homogeneous
import Vision.Camera
import Util.Misc(degree,vec,Mat,Vec)
import Util.Statistics(median)
import Util.Debug(impossible)
import Data.Maybe(catMaybes)


data KnownFs = AllKnown [Double] | F1Known Double | AllUnknown | ConstantUnknown

type PolarHorizon = (Double, Double)
type MetricInfo = (PolarHorizon, Double) 

extractInfo :: KnownFs -> [Mat] -> (Double,Double) -> (Mat, [Maybe Mat])

extractInfo (AllKnown fs) _ horiz = (c,mbOmegas) where
    mbOmegas = map (Just . omegaGen) (tail fs)
    c = camera0 (horiz, head fs)

extractInfo (F1Known f1) _ horiz = (c,mbOmegas) where
    mbOmegas = repeat Nothing -- :: [Maybe (Matrix Double)]
    c = camera0 (horiz,f1)

extractInfo AllUnknown hs horiz = (c,mbOmegas) where
    mf1s = map (estimateFTransfer horiz) hs
    mf1 = estimatorF mf1s
    c = case mf1 of 
            Just f1 -> camera0 (horiz,f1)
            Nothing -> ident 3
    mbOmegas = repeat Nothing :: [Maybe (Matrix Double)]

extractInfo ConstantUnknown hs horiz = (c,mbOmegas) where
    mf1s = map (estimateFTransfer horiz) hs
    mf1 = estimatorF mf1s
    c = case mf1 of 
            Just f1 -> camera0 (horiz,f1)
            Nothing -> ident 3
    mbOmega1 = mf1 >>= \f -> Just (omegaGen f)
    mbOmegas = repeat mbOmega1


consistency :: KnownFs -> [Mat] -> PolarHorizon -> Double

consistency info hs horiz = r where
    ihs = map inv hs
    (c,mbOmegas) = extractInfo info hs horiz
    r = quality ihs mbOmegas c


quality :: [Mat] -> [Maybe Mat] -> Mat -> Double
quality ihs mbOmgs c = sum qs / fromIntegral (length ihs) where 
    camscorr = map (<>c) ihs
    qs = zipWith autoOrthogonality mbOmgs camscorr


-- this gives a low value if h is a similar transformation
similarityDegree :: Mat -> Double
similarityDegree h = pnorm PNorm1 (m'-v) where
    v = vec [1,0,0,0,1,0,0,0,0]
    m = flatten (h <> mS <> trans h)
    m' = m / scalar (m@>0)


omegaGen :: Double -> Mat
omegaGen f = kgen (recip (f*f))

-- this gives a measure of the difference with a camera homography, for known f
orthogonality :: Mat -> Mat -> Double
orthogonality omega c = pnorm PNorm1 (m'-v) where
    v = vec [1,0,0,1]
    m = flatten $ subMatrix (0,0) (2,2) q
    m' = m / scalar (m@>0)
    q = trans c <> omega <> c

-- if given f (omega) it uses it, otherwise we try to estimate it. If not possible we check if this is a similar transformation.
autoOrthogonality :: Maybe Mat -> Mat -> Double
autoOrthogonality mbOmega c = res where
    res = case mbOmega of
            Just omega -> orthogonality omega c
            Nothing -> auto
    auto = case focalFromHomogZ0 c of
            Just f -> orthogonality (omegaGen f) c
            Nothing -> similarityDegree c

{-
-- rectifier transformation
rectifier ((rho,yh),f) = kgen f <> rot1 (atan2 f yh) <> rot3 (-rho) <> kgen (recip f)
-}

--camera0 ((rho,yh),f) = kgen f <> rot3 rho <> rot1 (- atan2 f yh) <> kgen (recip f)
--camera0 = inv.rectifier 
-- | true camera (the inverse of the rectifying transformation)
camera0 :: MetricInfo -> Mat
camera0 ((rho,yh),f) = (3 >< 3) [
      cr, -ca*sr, -f*sa*sr,
      sr,  ca*cr,  f*cr*sa,
      0,  -sa/f ,  ca ]
    where a = - atan2 f yh
          ca = cos a
          sa = sin a
          cr = cos rho
          sr = - sin rho      

{-
northPoint c = c <> mS <> trans c <> linf    
    
ryf c = focalFromHomogZ0 c >>= \f -> Just ((rho,yh),f) where
    [x,y,w] = toList $ northPoint $ c   
    rho = atan2 x y
    yh = sqrt (nx*nx+ny*ny)
    nx = x/w
    ny = y/w
-}    
    

polarHoriz :: PolarHorizon -> Vec
polarHoriz (r',y) = h where
    r = -(r'+3*pi/2)
    n = vec [y* cos r, y* sin r , 1]
    h = cross n (mA<>n)
    
  
-- estimation of f1 given a polar horiz (r,y) and a interimage homograpy 1<-k
estimateFTransfer :: PolarHorizon -> Mat -> Maybe Double
estimateFTransfer (r,y) h = res 
  where
    horiz = polarHoriz (r,y) -- horiz in view 1
    hp = trans h <> horiz  -- horiz in view k
    d = mA <> hp
    n = cross hp d 
    a = inHomog $ unitary $ h <> n
    b = inHomog $ unitary $ h <> d
    ni = inHomog $ unitary $ cross horiz (mA <> horiz)
    yh = norm ni
    x1 = norm (a-ni)
    x2 = norm (b-ni)
    f = sqrt (x1*x2-yh*yh)
    res = if f > 0.5 && f < 10 && x1 < 20 &&  x2 < 20 -- parametrizar mejor
            then Just f
            else Nothing
 


            
-- a partir de la distribución de posibles estimaciones da la mediana o Nothing...
estimatorF :: [Maybe Double] -> Maybe Double
estimatorF = mbMedian . catMaybes  
  where
    mbMedian :: Ord a => [a] -> Maybe a
    mbMedian l | null l    = Nothing
               | otherwise = Just (median l)


-------------------------------------------------------

-- | for a cost function based on 'consistency'
findSol :: (PolarHorizon -> Double) -> (Double, Double) -> ([Double], Mat)
findSol fun (rinit,hinit) = minimize NMSimplex2 1e-6 500 [0.1*degree,0.01] (mkfun fun) [rinit,hinit]
  where
    mkfun f = g where
        g [a,b] = f (a,b)
        g _ = impossible "mkfun"

