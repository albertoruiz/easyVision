
import Numeric.LinearAlgebra
import Util.Misc(Mat,degree,debug,vec,Vec)
import Data.Maybe(fromJust)
import Vision.Bootstrap
import Util.Rotation

-- the maximum angle in the axis is very close to the norm of the tangent vector.
angularTest p t r = do
    let c = rot1 (p*degree) <> rot2 (t*degree)<>rot3 (r*degree)
    print $ (pnorm PNorm2 $ fromList $ coordsRot c) / degree
    print $ angularError (ident 3) c

-- equal to angularError (ident 3) (x<>y^T)
angularError x y = maximum $ zipWith aang xs ys
      where
        xs = toRows x
        ys = toRows y
        aang u v = acos (min 1 $ abs $ u <.> v) / degree


------------------------------------------------------------------
-- testing data

gr a b c = rot3 (c*degree) <> rot2 (b*degree) <> rot1 (a*degree)

tr 0 = gr 0 0 0
tr 1 = gr 30 40 50
tr 2 = gr (-10) 20 (-30)
tr 3 = gr 55 (-30) 45

dr 0 = gr 1 (-2) 3
dr 1 = gr 5 5 (-7)
dr 2 = gr 0 8 2
dr 3 = gr (-5) 4 (-2)

br k = tr k <> dr k

obs = [(0,1),(0,2),(1,2),(1,3),(2,3)]
--obs = [(0,1),(1,2),(2,3)]

residualRotsTesting br = (br, map f obs) where
    f (i,j) = ((i,j), coordsRot $ trans (br j) <> rij <> br i) where rij = tr j <> trans (tr i)

lieStepTest br = ropt where
    (r0',resi) = residualRotsTesting br
    (coef,desi) = makeEcs resi
    (sol,err) = solveEcs 1 resi
    r0 = map r0' [0..(maximum $ map snd obs)]
    ropt' = zipWith fixRot r0 (toLists sol)
    ropt = ((map (<>trans (ropt'!!0)) ropt')!!)

ropt = lieStepTest br
ropt2 = lieStepTest ropt
ropt3 = lieStepTest ropt2

err0 = sum $ zipWith angularError (map tr [0..3]) (map (<> trans (br 0)) (map br [0..3]))
err1 = sum $ zipWith angularError (map tr [0..3]) (map ropt [0..3])
err2 = sum $ zipWith angularError (map tr [0..3]) (map ropt2 [0..3])
err3 = sum $ zipWith angularError (map tr [0..3]) (map ropt3 [0..3])

