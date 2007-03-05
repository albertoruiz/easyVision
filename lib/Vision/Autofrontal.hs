-----------------------------------------------------------------------------
{- |
Module      :  Vision.Autofrontal
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Practical Planar Metric Rectification.
Ruiz et al. BMVC06

-}
-----------------------------------------------------------------------------

module Vision.Autofrontal (
    camera0,
    KnownFs(..),
    extractInfo,
    consistency,
    findSol
) where

-- experiments on planar rectification

import GSL hiding (Matrix, Vector)
import qualified GSL as G
import Vision.Geometry
import Data.List (elemIndex,sort)
import Debug.Trace
import System.Random
import System.Environment (getArgs)
import Control.Monad (when)

type Matrix = G.Matrix Double
type Vector = G.Vector Double

matrix = fromLists :: [[Double]] -> Matrix
vector = fromList ::  [Double] -> Vector

(!:) = (@>)

data KnownFs = AllKnown [Double] | F1Known Double | AllUnknown | ConstantUnknown


extractInfo :: KnownFs -> [Matrix] -> (Double,Double) -> (Matrix, [Maybe Matrix])

extractInfo (AllKnown fs) hs horiz = (c,mbOmegas) where
    mbOmegas = map (Just . omegaGen) (tail fs)
    c = rectifier' (horiz, head fs)

extractInfo (F1Known f1) hs horiz = (c,mbOmegas) where
    mbOmegas = repeat Nothing :: [Maybe Matrix]
    c = rectifier' (horiz,f1)

extractInfo AllUnknown hs horiz = (c,mbOmegas) where
    mf1s = map (estimateFTransfer horiz) hs
    mf1 = estimatorF mf1s
    c = case mf1 of 
            Just f1 -> rectifier' (horiz,f1)
            Nothing -> ident 3
    mbOmegas = repeat Nothing :: [Maybe Matrix]

extractInfo ConstantUnknown hs horiz = (c,mbOmegas) where
    mf1s = map (estimateFTransfer horiz) hs
    mf1 = estimatorF mf1s
    c = case mf1 of 
            Just f1 -> rectifier' (horiz,f1)
            Nothing -> ident 3
    mbOmega1 = mf1 >>= \f -> Just (omegaGen f)
    mbOmegas = repeat mbOmega1


consistency :: KnownFs -> [Matrix] -> (Double,Double) -> Double

consistency info hs horiz = r where
    ihs = map inv hs
    (c,mbOmegas) = extractInfo info hs horiz
    r = quality ihs mbOmegas c

quality ihs mbOmgs c = sum qs / fromIntegral (length ihs) where 
    camscorr = map (<>c) ihs
    qs = zipWith autoOrthogonality mbOmgs camscorr


-- this gives a low value if h is a similar transformation
similarityDegree h = pnorm 1 (m'-v) where
    v = vector [1,0,0,0,1,0,0,0,0]
    m = flatten (h <> mS <> trans h)
    m' = m <> recip (m!:0)

-- hmm! premature optimization...
omegaGen f = kgen (recip (f*f))

-- this gives a measure of the difference with a camera homography, for known f
orthogonality omega c = pnorm 1 (m'-v) where
    v = vector [1,0,0,1]
    m = flatten $ subMatrix (0,0) (2,2) q
    m' = m <> recip (m!:0)
    q = trans c <> omega <> c 

-- si das un f (omega) la usa, si no intenta estimarla y si no puede ve si es similar
autoOrthogonality mbOmega c = res where
    res = case mbOmega of
            Just omega -> orthogonality omega c
            Nothing -> auto
    auto = case focalFromHomogZ0 c of
            Just f -> orthogonality (omegaGen f) c
            Nothing -> similarityDegree c

-- rectifier transformation
-- should be precomputed
rectifier ((rho,yh),f) = kgen f <> rot1 (atan2 f yh) <> rot3 (-rho) <> kgen (recip f)

-- associated camera (the inverse of the above)
--rectifier' ((rho,yh),f) = kgen f <> rot3 rho <> rot1 (- atan2 f yh) <> kgen (recip f)
--rectifier' = inv.rectifier 
rectifier' ((rho,yh),f) = reshape 3 $ vector [
      cr, -ca*sr, -f*sa*sr,
      sr,  ca*cr,  f*cr*sa,
      0,  -sa/f ,  ca ]
    where a = - atan2 f yh
          ca = cos a
          sa = sin a
          cr = cos rho
          sr = - sin rho      

camera0 = rectifier'

northPoint c = c <> mS <> trans c <> linf    
    
ryf c = focalFromHomogZ0 c >>= \f -> Just ((rho,yh),f) where
    [x,y,w] = toList $ northPoint $ c   
    rho = atan2 x y
    yh = sqrt (nx*nx+ny*ny)
    nx = x/w
    ny = y/w
    
    

polarHoriz :: (Double,Double) -> Vector
polarHoriz (r',y) = h where
    r = -(r'+3*pi/2)
    n = vector [y* cos r, y* sin r , 1]
    h = cross n (mA<>n)
    
  
-- estimation of f1 given a polar horiz (r,y) and a interimage homograpy 1<-k
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
 


posMin l = i where
    Just i = elemIndex (minimum l) l

mbMedian l = m where
    n = length l
    m = if n == 0 
            then Nothing
            else Just (sort l !! (n`quot`2))
            
-- a partir de la distribución de posibles estimaciones da la mediana o Nothing...
estimatorF :: [Maybe Double] -> Maybe Double
estimatorF mbfs = mbMedian fs where
    fs = [ f | Just f <- mbfs ]    


-------------------------------------------------------

mkfun f = g where
    g [a,b] = f (a,b)
    
findSol fun (rinit,hinit) = minimizeNMSimplex (mkfun fun) [rinit,hinit] [0.1*degree,0.01] 1e-6 500
    
-- Numerical estimation of the gradient
gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v    

-- the conjugate gradient method
minimizeCG f df xi = minimizeConjugateGradient 1E-2 1E-4 1E-3 30

findSol' fun (rinit,hinit) = minimizeCG (mkfun fun) (gradient (mkfun fun)) [rinit,hinit] 

