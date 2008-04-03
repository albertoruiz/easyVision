-- check the UKF

import Numeric.LinearAlgebra
import Kalman

import Debug.Trace

debug x = trace (show x) x


vector x = fromList x :: Vector Double
diagl = diag . vector


main = do
    let m = 3 |> [1,1,1]
        c = diagl [1,1,1]
        t = (2><3) [1,1,0,0,1,1]
    print $ unscentedTransform ((2><3) [1,1,0,0,1,1] <>) (m,c)
    print $ (t<>m, t <> c <> trans t)
