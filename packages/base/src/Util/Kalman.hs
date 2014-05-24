{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Util.Kalman
-- Copyright   :  (c) Alberto Ruiz 2008
-- License     :  GPL-style
--
-- Maintainer  :  Alberto Ruiz <aruiz@um.es>
-- Stability   :  provisional
--
-- Kalman, Extended Kalman, and Unscented Kalman filters.
--
-----------------------------------------------------------------------------

module Util.Kalman (
    State(..),
    Measurement,
    LinearSystem(..),
    kalman,
    System(..),
    ekf,
    unscentedSamples,
    unscentedTransform,
    ukf, UKFParam(..), ukfDefaultParam
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Differentiation
import Util.Misc(Vec,Mat,vec,mat)

--import Debug.Trace
--import Text.Printf

-- debug x = trace (show x) x
-- debugM x = trace (format " " (printf "%2.0f") (100*abs x)) x
-- debugLV k x = trace (show (fromRows x)) x
-- debugS s x = trace (s++" = "++show x) x


-------------- Ordinary Kalman Filter -------------------------------

data LinearSystem = LinearSystem {kF, kH, kQ, kR :: Mat}
data State = State {sX :: Vec , sP :: Mat, nZ :: Vec } deriving Show
type Measurement = Vec

kalman' :: LinearSystem -> State -> Measurement -> State
kalman' (LinearSystem f h q r) (State x p _) z = State x' p' zp where
    px = f <> x                            -- prediction
    pq = f <> p <> trans f + q             -- its covariance
    zp = h <> px                           -- predicted measurement
    y  = z - zp                            -- residue
    cy = h <> pq <> trans h + r            -- its covariance
    k  = pq <> trans h <> inv cy           -- kalman gain
    x' = px + k <> y                       -- new state
    p' = (ident (dim x) - k <> h) <> pq    -- its covariance


blindKalman :: LinearSystem -> State -> State
blindKalman (LinearSystem f h q _r) (State x p _) = State x' p' (h <> x') where
    x' = f <> x
    p' = f <> p <> trans f + q

-- | Kalman filter update step
kalman :: LinearSystem -> State -> Maybe Measurement -> State
kalman sys st (Just m) = kalman'     sys st m
kalman sys st Nothing  = blindKalman sys st

-------------- Extended Kalman Filter -------------------------------

partialDerivative :: Int -> ([Double] -> Double) -> [Double] -> Double
partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v

--gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

jacobian :: ([Double] -> [Double]) -> [Double] -> [[Double]]
jacobian f v = [[partialDerivative k ((!!s).f) v | k <- [0 .. length v -1]] | s <- [0..length (f v) -1]]

--------------------------------------------------------------------

data System = System {ekF, ekH :: [Double] -> [Double],
                      ekQ, ekR :: Mat}

--data State = State {sX :: [Double] , sP :: Matrix Double} deriving Show
--type Measurement = Vector Double

ekf' :: System -> State -> Measurement -> State
ekf' (System f h q r) (State vx p _) z = State x' p' zp where
    x = toList vx
    jf = mat (jacobian f x)
    jh = mat (jacobian h x)

    px = f x                               -- prediction
    pq = jf <> p <> trans jf + q           -- its covariance
    zp = vec (h px)                     -- predicted measurement
    y  = z - zp                            -- residue
    cy = jh <> pq <> trans jh + r          -- its covariance
    k  = pq <> trans jh <> inv cy          -- kalman gain
    x' = vec px + k <> y                -- new state
    p' = (ident (dim x') - k <> jh) <> pq  -- its covariance


blindEKF :: System -> State -> State
blindEKF (System f h q _r) (State vx p _) = State (vec x') p' (vec (h x')) where
    x = toList vx
    x' = f x
    p' = jf <> p <> trans jf + q
    jf = mat (jacobian f x)

-- | Extended Kalman Filter update step
ekf :: System -> State -> Maybe Measurement -> State
ekf sys st (Just m) = ekf'     sys st m
ekf sys st Nothing  = blindEKF sys st

---------------------------------------------------------------------

data UKFParam = UKFParam { ukfAlpha :: Double
                         , ukfBeta  :: Double
                         , ukfKappa :: Int -> Double }

ukfDefaultParam :: UKFParam
ukfDefaultParam  = UKFParam { ukfAlpha = 0.5
                            , ukfBeta  = 2
                            , ukfKappa = \n -> 3 - fromIntegral n }

unscentedSamples :: UKFParam -> (Vec, Mat) -> ([Vec], ([Double], [Double]))
unscentedSamples UKFParam {..} (med,cov) = (med : concat [pos,neg], (wm,wc)) where
    pos = f (+)
    neg = f (-)
    f op = map (op med) ds
    ds = toRows $ mr ( (fromIntegral n + lambda) `scale` cov :: Matrix Double)
    wm = lambda/(nr+lambda) : ws
    wc = (lambda/(nr+lambda) + 1- ukfAlpha**2 + ukfBeta) : ws
    ws = replicate (2*n) (1/2/(nr+lambda))
    lambda = ukfAlpha**2 * (nr + ukfKappa n) - nr
    n  = dim med
    nr = fromIntegral n
    --mr m = trans $ v <> diag (sqrt l) where (l,v) = eigSH m
    mr = cholSH -- no symmetry check


unscentedTransform :: UKFParam -> (Vec -> Vec) -> (Vec, Mat) -> (Vec, Mat)
unscentedTransform param f = fst. unscentedTransformWithSamples param f

unscentedTransformWithSamples
    :: UKFParam -> (Vec -> Vec) -> (Vec, Mat)
    -> ((Vec, Mat),([Vec],([Double], [Double])))
unscentedTransformWithSamples param f g = ((m',c'),(s',w)) where
    (s, w@(wm,wc)) = unscentedSamples param g
    s' = map f s
    m' = fromList wm <> fromRows s'
    c' = sum (zipWith h wc s') where h ww v = ww `scale` outer vm vm where vm = v - m'


ukf' :: UKFParam -> System -> State -> Measurement -> State
ukf' param (System f h q r) (State vx p _) z = State x' p' mz where
    f' = fromList . f . toList
    h' = fromList . h . toList
    ((px,pc),(sx,(_,wc))) = unscentedTransformWithSamples param f' (vx,p)  -- prediction
    pq = pc + q                            -- its covariance

    ((mz,cz),(sz,_)) = unscentedTransformWithSamples param h' (px,pq) --predicted measurement
    y  = z - mz                            -- residue
    cy = cz + r                            -- its covariance

    cross = sum (zipWith3 hh wc sx sz)    -- !!!
        where hh w x zz = w `scale` outer (x-px) (zz-mz)

    k  = cross <> inv cy                   -- kalman gain
    x' = px + k <> y                       -- new state
    p' = pq - k <> cy <> trans k           -- its covariance


blindUKF :: UKFParam -> System -> State -> State
blindUKF param (System f h q _r) (State vx p _) = State px pq (h' px) where
    f' = fromList . f . toList
    h' = fromList . h . toList
    (px,pc) = unscentedTransform param f' (vx,p)  -- prediction
    pq = pc + q                                   -- its covariance

-- | Unscented Kalman Filter update step
ukf :: UKFParam -> System -> State -> Maybe Measurement -> State
ukf param sys st (Just m) = ukf'     param sys st m
ukf param sys st Nothing  = blindUKF param sys st
