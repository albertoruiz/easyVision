-----------------------------------------------------------------------------
{- |
Module      :  Util.Gaussian
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Bayesian inference for gaussians.

EM estimation for mixtures of gaussians.

-}
-----------------------------------------------------------------------------

module Util.Gaussian (
   -- * Single gaussian
   Gaussian(..),
   gaussianLogLik,
   marginal,
   conditional,
   jointLinear,
   bayesGaussianLinear,
   bayesGaussianLinearK,
   -- * Mixtures of gaussians
   Mixture,
   mixturePDF,
   sampleMixture,
   em, emSeq,
   findMixture,
   -- * Misc
   ellipCov2D
) where

import Numeric.LinearAlgebra
import Util.Misc(mat,Mat,Vec,Seed)
import Control.Arrow((***))
import Data.List(sortBy)
import Data.Function(on)
import Util.Optimize(optimize)
import Util.Geometry
import Util.Misc(degree)
import Util.Homogeneous(desp)
import Util.Rotation(rot3)


data Gaussian = N { mG :: Vec, cG :: Mat } deriving Show

extractVarsM ::[Int] -> Mat -> Mat
extractVarsM vs = trans . extractRows vs . trans . extractRows vs

extractVarsV :: [Int] -> Vec -> Vec
extractVarsV vs x = fromList $ map (x@>) vs

marginal :: [Int] -> Gaussian -> Gaussian
marginal vs (N m c) = N m' c'
  where
    m' = extractVarsV vs m
    c' = extractVarsM vs c

-- inputs = x y
conditional :: Vec -> Gaussian -> Gaussian
conditional y (N m c) = N m' c'
  where
    vs = [dim m - dim y, dim y]
    mx = subVector 0 (dim m - dim y) m
    my = subVector (dim mx) (dim y) m
    [[cxx,cxy],
     [cyx,cyy]] = toBlocks vs vs c
    r = cxy <> inv cyy
    m' = mx + r <> (y-my)
    c' = cxx - r <> cyx

-- a linear transformation of x is convolved with (suffers additive noise)
-- with offset o and covariance r 
-- x ~ N m c,   y|x ~ N (h x + o,  r)
jointLinear :: Gaussian -> Mat -> Gaussian -> Gaussian
jointLinear (N m c) h (N o r) = N m' c'
  where
    m' = vjoin [m, h <> m + o]
    cxy = c <> trans h
    cyy = h <> cxy + r
    c' = fromBlocks [[ c        , cxy ]
                    ,[ trans cxy, cyy ]]

-- direct, alternative method
bayesGaussianLinear :: Vec -> Gaussian -> Mat -> Gaussian -> Gaussian
bayesGaussianLinear y (N m c) h (N o r) = N m' c'
  where
    ir = inv r
    ic = inv c
    c' = inv (ic + trans h <> ir <> h)
    m' = c' <> (trans h <> ir <> (y - o) + ic <> m)

-- kalman-style method
bayesGaussianLinearK :: Vec -> Gaussian -> Mat -> Gaussian -> Gaussian
bayesGaussianLinearK y (N m c) h (N o r) = N m' c'
  where
    k = c <> trans h <> inv (h <> c <> trans h + r)
    c' = (ident (dim m) - k <> h) <> c
    m' = m + k <> (y - h <> m - o)

----------------------------------------------------------------------

-- | gaussian pdf = exp . gaussianLogLik g
gaussianLogLik :: Gaussian -> Vec -> Double
gaussianLogLik (N m c) = f
  where
    (ic,(lad,_)) = invlndet c
    k = fromIntegral (dim m) * log (2*pi) + lad
    f z = -0.5*(k+dM2 z)
      where
        dM2 x = xm <> ic <> xm
          where xm = x - m

----------------------------------------------------------------------

type Mixture = [(Double,Gaussian)]

sampleMixture :: [Seed] -> Int -> Mixture -> Mat
sampleMixture seeds n mix = fromBlocks $ map return $ zipWith3 f seeds ns (map snd mix)
  where
    ns = map (round . (fromIntegral n*). fst) mix
    f seed k (N m c) = gaussianSample seed k m c 

----------------------------------------------------------------------

eStep :: Mat -> (Mixture,Double) -> (Mat,Double)
eStep dat (mix,_) = (probs,loglik)
  where
    xs = toRows dat
    fs = map (\(w,g)-> ((*w). exp . gaussianLogLik g)) mix
    liks = mat [ [ f x | f <- fs ] | x <- xs ]
    sumRows = liks <> constant 1 (cols liks)
    probs = liks / asColumn sumRows
    loglik = (sumElements $ log sumRows) / fromIntegral (rows dat)

mStep :: Mat -> (Mat,Double) -> (Mixture,Double)
mStep dat (probs,l) = (mix,l)
  where
    n = fromIntegral $ rows dat
    mix = zipWith3 wg ws ms cs
      where wg w m c = (w, N m c)

    ps = map asColumn (toColumns probs)
    ws = toList $ (fst.meanCov) probs
    wis = map recip ws
    
    ms' = map (fst.meanCov) (map (dat*) ps)
    ms = zipWith scale wis ms'

    cs = zipWith3 f ws ps ms
      where f w p m = scale (recip (w*n)) t
              where xc = dat - asRow m
                    t = trans (xc * p) <> xc

em :: Mat -> Mixture -> (Mixture,Double)
em dat mix = fst $ -- debug "EM lik" snd $
    (mStep dat *** id) $
    optimize 0 0.1 20 
    (eStep dat . mStep dat)
    (negate.snd)
    (eStep dat (mix,undefined))

----------------------------------------------------------------------

initMix1 :: Mat -> Mixture
initMix1 dat = [(1,N m c)] where (m,c) = meanCov dat 

diviG :: (Double, Gaussian) -> [(Double, Gaussian)]
diviG (k,(N m c)) = [(0.5*k,N m1 c),(0.5*k, N m2 c)]
  where
    (l,v) = eigSH' c
    l1 = 2*sqrt (l@>0)
    v1 = l1 `scale` (head $ toColumns v)
    m1 = m + v1
    m2 = m - v1

-- split the most probable component
augmentMix :: Mixture -> Mixture
augmentMix mix = diviG g ++ gs
  where
    g:gs = sortBy (compare `on` (negate.fst)) mix

emSeq :: Mat -> [(Mixture,Double)]
emSeq dat = iterate (em dat . augmentMix . fst) (m0, snd $ eStep dat (m0,undefined))
  where
    m0 = initMix1 dat

mdl :: Mat -> (Mixture, Double) -> Double
mdl dat (mix,lik) = -lik * n + (k * np - 1)/2* log n
  where
    k = fromIntegral (length mix)
    n = fromIntegral (rows dat)
    d = fromIntegral $ dim (mG $ snd $ head mix)
    np = 1 + d + d*(d-1)/2

findMixture :: Mat -> Mixture
findMixture dat = fst $ ms!!k
  where
    ms = emSeq dat
    qs = map (mdl dat) ms
    f j a b = (j,b-a)
    qqs = zipWith3 f [0..] qs (tail qs)
    k = fst $ head $ snd $ break ((>0).snd) qqs

mixturePDF :: Mixture -> Vec -> Double
mixturePDF mix = f
  where
    gs = [ (w*) . exp . gaussianLogLik g | (w,g) <- mix ]
    f x = sum (map ($x) gs)

--------------------------------------------------------------------------------

ellipCov2D :: Double -> Gaussian -> Polyline
ellipCov2D σ (N m c) = h ◁ circle
  where
    angle = atan2 vy vx
    (l,v) = eigSH' c
    [d1,d2] = toList (sqrt l)
    [vx,vy] = toList $ head (toColumns v)
    circle = Closed [Point (σ*d1*cos(t*degree)) (σ*d2*sin(t*degree)) | t <- [ 0, 10 .. 350 ]]
    h = unsafeFromMatrix $ desp (m@>0,m@>1) <> rot3 (-angle) :: Homography

