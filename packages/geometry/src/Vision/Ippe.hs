-- TODO: header comments

module Vision.Ippe (
h2p, ippe
)
where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.HMatrix

htest = (3><3) 
   [ 0.0938,    0.0829,    0.0728
   ,-0.0291,    0.0771,    0.0052
   , 0.0857,   -0.0645,    1.0000] :: Matrix Double


-- | Obtain pose from homography using Infinitesimal Plane Pose Estimation
h2p :: Matrix Double -> (Matrix Double, Matrix Double)
h2p h = (r1 ||| t1, r2 ||| t2)
  where
    (gamma, r1, r2) = ippe v j
    v = col [h!0!2, h!1!2]
    j = reshape 2 (fromList [h!0!0 - h!2!0*h!0!2, h!0!1 - h!2!1*h!0!2,
                             h!1!0 - h!2!0*h!1!2, h!1!1 - h!2!1*h!1!2])
    t1 = scalar(1/gamma) * (v === 1)
    t2 = scalar(1/gamma) * (v === 1)


ippe :: Matrix Double -> Matrix Double -> (Double, Matrix Double, Matrix Double)
ippe v j = (gamma, r1, r2)
  where
    r1 = rv <> ((r22 ||| cc) === (tr bb ||| aa))
    r2 = rv <> ((r22 ||| -cc) === (-tr bb ||| aa))
    r22 = scalar(1/gamma) * a
    ca = cross (flatten ((r22 === tr bb) <> col [1.0, 0.0])) (flatten ((r22 === tr bb) <> col [0.0, 1.0]))
    cc = ((col . toList) ca) ? [0,1]
    aa = scalar (ca ! 2)
    bb = col [sqrt rr!0!0, (signum (rr!0!1))*(sqrt rr!1!1)]
    rr = ident 2 - (tr r22) <> r22
    gamma = e ! 0;
    e = singularValues a
    a = (inv b) <> j
    b = ((ident 2 ||| -v) <> rv) ¿ [0,1]
    rv = ident 3 + scalar (sT) * k + scalar(1-cT) * (k <> k)
    sT = sqrt (1-1/s**2)
    k = (scalar (1/t)) * ((konst 0 (2,2) ||| v) === (-tr v ||| 0))
    cT = 1/s
    s = (norm_2 (v === 1))
    t = norm_2 v

