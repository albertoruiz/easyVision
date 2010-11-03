module LieSolve where

import Numeric.LinearAlgebra
import Util.Misc(Mat,degree,debug,vec)
import Data.Maybe(fromJust)


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

-------------------------------------------------------------
-- best fit of rotations in tangent space



makeEc n (i,j) = replicate i 0 ++ [1] ++ replicate (j-i-1) 0 ++ [-1] ++ replicate (n-j) (0::Double)

makeEcs resi = (fromLists coef, fromLists desi)
    where coef = map (makeEc n . fst) resi
          n = maximum $ map (snd.fst) resi
          desi = map snd resi

solveEcs resi = (sol',err)
  where
    (c', d) = makeEcs resi
    c = dropColumns 1 c'
    sol' = fromRows $ vec [0,0,0] : toRows sol
    sol = debug "system: " f c `linearSolveSVD` d
    err = (norm (c <> sol - d), norm d)
    norm x = sqrt (pnorm Frobenius x ** 2 / fromIntegral (rows x)) / degree
    f m = (rows m, cols m, rank m)

fixRot r [a,b,c] = r <> trans dr
    where --dr = expm (scalar a * g1 |+| scalar b * g2 |+| scalar c * g3 )
          dr = expm (scalar a * g1 + scalar b * g2 + scalar c * g3)

-----------------------------------------------------------------------------------

logm m = fst . fromComplex . matFunc log . complex $ m

rot1, rot2, rot3 :: Double -> Mat
rot1 a = (3><3)
 [ 1, 0, 0
 , 0, c, s
 , 0,-s, c ]
    where c = cos a
          s = sin a

rot2 a = (3><3)
 [ c, 0, s
 , 0, 1, 0
 ,-s, 0, c ]
    where c = cos a
          s = sin a

rot3 a = (3><3)
 [ c, s, 0
 ,-s, c, 0
 , 0, 0, 1 ]
    where c = cos a
          s = sin a


g1,g2,g3 :: Mat

g1 = (3><3) [0, 0,0
            ,0, 0,1
            ,0,-1,0]

g2 = (3><3) [ 0,0,1
            , 0,0,0
            ,-1,0,0]

g3 = (3><3) [ 0,1,0
            ,-1,0,0
            , 0,0,0]

coordsRot r = [a,b,c] where
    [[_,c,b],
     [_,_,a],
     [_,_,_]] = toLists (logm r)

infix 8 ~&~
a ~&~ b = a <> b - b <> a

infixl 6 |+|
a |+| b = a + b + a ~&~ b /2  + (a-b) ~&~ (a ~&~ b) /12

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

residualRotsTesting = (br, map f obs) where
    f (i,j) = ((i,j), coordsRot $ trans (br j) <> rij <> br i) where rij = tr j <> trans (tr i)

(r0',resi) = residualRotsTesting
(coef,desi) = makeEcs resi
(sol,err) = solveEcs resi
r0 = map r0' [0..(maximum $ map snd obs)]
ropt' = zipWith fixRot r0 (toLists sol)
ropt = ((map (<>trans (ropt'!!0)) ropt')!!)

err0 = sum $ zipWith angularError (map tr [0..3]) (map (<> trans (r0!!0)) r0)
err1 = sum $ zipWith angularError (map tr [0..3]) (map ropt [0..3])

