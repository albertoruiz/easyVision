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
    ukf,
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Differentiation
import Debug.Trace
import Text.Printf

debug x = trace (show x) x
debugM x = trace (format " " (printf "%2.0f") (100*abs x)) x
debugLV k x = trace (show (fromRows x)) x
debugS s x = trace (s++" = "++show x) x

--------------------------------------------------------------------

vector l = fromList l :: Vector Double
matrix ls = fromLists ls :: Matrix Double
diagl = diag . vector

-------------- Ordinary Kalman Filter -------------------------------

data LinearSystem = LinearSystem {kF, kH, kQ, kR :: Matrix Double}
data State = State {sX :: Vector Double , sP :: Matrix Double, nZ :: Vector Double } deriving Show
type Measurement = Vector Double

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
blindKalman (LinearSystem f h q r) (State x p _) = State x' p' (h <> x') where
    x' = f <> x
    p' = f <> p <> trans f + q

-- | Kalman filter update step
kalman :: LinearSystem -> State -> Maybe Measurement -> State
kalman sys st (Just m) = kalman'     sys st m
kalman sys st Nothing  = blindKalman sys st

-------------- Extended Kalman Filter -------------------------------

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v

gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

jacobian f v = [[partialDerivative k ((!!s).f) v | k <- [0 .. length v -1]] | s <- [0..length (f v) -1]]

--------------------------------------------------------------------

data System = System {ekF, ekH :: [Double] -> [Double],
                      ekQ, ekR :: Matrix Double}

--data State = State {sX :: [Double] , sP :: Matrix Double} deriving Show
--type Measurement = Vector Double

ekf' :: System -> State -> Measurement -> State
ekf' (System f h q r) (State vx p _) z = State x' p' zp where
    x = toList vx
    jf = matrix (jacobian f x)
    jh = matrix (jacobian h x)

    px = f x                               -- prediction
    pq = jf <> p <> trans jf + q           -- its covariance
    zp = vector (h px)                     -- predicted measurement
    y  = z - zp                            -- residue
    cy = jh <> pq <> trans jh + r          -- its covariance
    k  = pq <> trans jh <> inv cy          -- kalman gain
    x' = vector px + k <> y                -- new state
    p' = (ident (dim x') - k <> jh) <> pq  -- its covariance


blindEKF :: System -> State -> State
blindEKF (System f h q r) (State vx p _) = State (vector x') p' (vector (h x')) where
    x = toList vx
    x' = f x
    p' = jf <> p <> trans jf + q
    jf = matrix (jacobian f x)

-- | Extended Kalman Filter update step
ekf :: System -> State -> Maybe Measurement -> State
ekf sys st (Just m) = ekf'     sys st m
ekf sys st Nothing  = blindEKF sys st

---------------------------------------------------------------------

alpha = 0.01 -- parameter??
beta = 2
k = 0

unscentedSamples (med,cov) = (med : concat [pos,neg], (wm,wc)) where
    pos = f (+)
    neg = f (-)
    f op = map (op med) ds
    ds = toColumns $ mr ( (fromIntegral n + lambda) .* cov :: Matrix Double)
    wm = fromList $ lambda/(fromIntegral n+lambda) : ws
    wc = (lambda/(fromIntegral n+lambda) + 1-alpha^2+beta) : ws
    ws = replicate (2*n) (1/2/(fromIntegral n+lambda))
    lambda = alpha^2 * (fromIntegral n + k) - fromIntegral n
    n = dim med
    --mr m = v <> diag (sqrt l) where (l,v) = eigSH m
    mr = trans . cholSH                       -- no symmetry check


unscentedTransform f = fst. unscentedTransformWithSamples f

unscentedTransformWithSamples f g = ((m',c'),(s',w)) where
    (s, w@(wm,wc)) = unscentedSamples g
    s' = map f s
    m' = wm <> fromRows s'
    c' = sum (zipWith f wc s') where f w v = w .* outer vm vm where vm = v - m'


ukf' :: System -> State -> Measurement -> State
ukf' (System f h q r) (State vx p _) z = State x' p' mz where
    f' = fromList . f . toList
    h' = fromList . h . toList
    ((px,pc),(sx,(_,wc))) = unscentedTransformWithSamples f' (vx,p)  -- prediction
    pq = pc + q                            -- its covariance

    ((mz,cz),(sz,_)) = unscentedTransformWithSamples h' (px,pq) --predicted measurement
    y  = z - mz                            -- residue
    cy = cz + r                            -- its covariance

    cross = sum (zipWith3 f wc sx sz)    -- !!!
        where f w x z = w .* outer (x-px) (z-mz)

    k  = cross <> inv cy                   -- kalman gain
    x' = px + k <> y                       -- new state
    p' = pq - k <> cy <> trans k           -- its covariance


blindUKF :: System -> State -> State
blindUKF (System f h q r) (State vx p _) = State px pq (h' px) where
    f' = fromList . f . toList
    h' = fromList . h . toList
    (px,pc) = unscentedTransform f' (vx,p)  -- prediction
    pq = pc + q                             -- its covariance

-- | Unscented Kalman Filter update step
ukf :: System -> State -> Maybe Measurement -> State
ukf sys st (Just m) = ukf'     sys st m
ukf sys st Nothing  = blindUKF sys st
