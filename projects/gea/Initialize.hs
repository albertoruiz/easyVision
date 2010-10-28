module Initialize where

import LieSolve

import Numeric.LinearAlgebra
import Util.Misc(Mat)
import Vision(factorizeCamera,camerasFromEssential,selectCamera,cameraAtOrigin,inHomog,
              SparseVP,ako,essentials,qEssen)

-- initialization of rotations in a sparse visual problem

rotOfCam c = r where (_,r,_) = factorizeCamera c

-- should return both possibilities
extractRotation esen p p' = r where
    ms = camerasFromEssential esen
    m' = selectCamera (f p) (f p') cameraAtOrigin ms
    r = rotOfCam m'
    f = toList . inHomog -- we should also admit triangulation of homog points

relativeRotations :: SparseVP -> [((Int, Int), (Mat, [Int]))] -> [((Int, Int), Mat)]
relativeRotations s = map f where
    f ((i,j), (e, k:_)) = ((i,j), r)
        where r = extractRotation e p p'
              p  = ako s (k,i)
              p' = ako s (k,j)

initRots :: [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots ps = (r0, r)
  where (r0',resi) = residualRots ps
        (sol,err) = solveEcs resi
        r0 = map r0' [0..maximum (map (snd.fst) ps) -1]
        r = zipWith fixRot r0 (toLists sol)

initRots1I :: Mat -> [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots1I b ps = (f r0, f r)
  where (r0,r) = initRots ps
        f rs = map (<> (trans (head rs) <> b)) rs

prepareRotsGen q p = initRots1I cameraAtOrigin . relativeRotations p . filter q . essentials $ p

-- using only pair with minimum s2
prep s2 = prepareRotsGen (\(_,(e,ks)) -> fst (qEssen e) > s2)

