module Vision.Bootstrap(
    -- * Rotations
    makeEcs,
    solveEcs,
    coordsRot,
    fixRot,
    -- * Centers
    solveCams,
    estimatePointsCenters
) where

import Util.Rotation
import Vision.Epipolar
import Util.Misc(vec,Mat,Vec,debug,degree,impossible,(&),arrayOf,unionSort,unitary)
import Numeric.LinearAlgebra hiding (i)
import Numeric.LinearAlgebra.Tensor hiding (scalar)
import Numeric.LinearAlgebra.Array.Util hiding (scalar)
import Vision.Types
import Util.Estimation(homogSolve,compact)
import Data.List(sortBy)
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Homogeneous(Point(..))

----------------------------------------------------------------------

-- best fit of rotations in tangent space

makeEc :: Int -> (Int, Int) -> [Double]
makeEc n (i,j) = replicate i 0 ++ [1] ++ replicate (j-i-1) 0 ++ [-1] ++ replicate (n-j) (0::Double)

makeEcs :: [((Int, Int), [Double])] -> (Mat, Mat)
makeEcs resi = (fromLists coef, fromLists desi)
    where coef = map (makeEc n . fst) resi
          n = maximum $ map (snd.fst) resi
          desi = map snd resi

solveEcs :: Vec -- ^ residual weigths
         -> [((Int,Int),[Double])] -- ^ residuals
         -> (Mat, (Double, Double))
solveEcs w resi = (sol',err)
  where
    (c', d) = makeEcs resi
    c = dropColumns 1 c'
    sol' = fromRows $ vec [0,0,0] : toRows sol
    sol = weight (debug "rotation system size: " f c) `linearSolveSVD` (weight d)
    err = (norm (c <> sol - d), norm d)
    norm x = sqrt (pnorm Frobenius x ** 2 / fromIntegral (rows x)) / degree
    f m = (rows m, cols m, rank m)
    weight m = asColumn w * m

fixRot :: Mat -> [Double] -> Mat
fixRot r [a,b,c] = r <> trans dr
    where --dr = expm (scalar a * g1 |+| scalar b * g2 |+| scalar c * g3 )
          dr = expm (scalar a * rot1g + scalar b * rot2g + scalar c * rot3g)
fixRot _ _ = impossible "fixRot"

coordsRot :: Mat -> [Double]
coordsRot r = [a,b,c] where
    [[_,c,b],
     [_,_,a],
     [_,_,_]] = toLists (logm r)

    logm m = fst . fromComplex . matFunc log . complex $ m


{-
infix 8 ~&~
a ~&~ b = a <> b - b <> a

infixl 6 |+|
a |+| b = a + b + a ~&~ b /2  + (a-b) ~&~ (a ~&~ b) /12
-}

----------------------------------------------------------------------

-- linear estimation of camera centers from known rotations


solveCams :: [Mat] -- ^ Rotations
          -> EpiPairs
          -> (Motion, Motion) -- ^ two solutions
solveCams rs sel = (cams1, cams2) where
    cens = fst $ debug "centers system error: " snd $ estimateCenters rs sel
    cams1 = zipWith f rs cens
    cams2 = zipWith f rs (map negate cens)
    f r c = r & asColumn (-r <> c)


estimateCenters :: [Mat] -> EpiPairs -> ([Vec], Double)
estimateCenters rots = p . homogSolve . debug "centers system: " f . dropColumns 3 . coeffCent rots . mapSnd m_hat
    where p (s,err) = ((vec[0,0,0]:) . toRows . reshape 3 $ s, err)
          f m = (rows m, cols m, rank m)

mapSnd :: (t1 -> t2) -> [(t, t1)] -> [(t, t2)]
mapSnd f = map (\(a,b)->(a,f b))

-- | create full coefficient matrix
coeffCent :: [Mat] -> [((Int, Int), Mat)] -> Mat
coeffCent rots = fromBlocks . map (return . f) . unRotEpi rots
    where f ((i,j),m) = ec i j m
          n = length rots - 1
          ec i j b = fromBlocks [ replicate i z
                               ++ [b]
                               ++ replicate (j-i-1) z
                               ++ [-b]
                               ++ replicate (n-j) z
                                ]
              where z = (rows b >< 3) [0,0..]


unRotEpi :: [Mat] -- ^ Rotations
         -> [((Int, Int), Mat)] -- ^ original M hat 
         -> [((Int, Int), Mat)] -- ^ unrotated and reduced to 2x3
unRotEpi rots = map f
    where r = arrayOf $ map (listArray[3,3].toList.flatten .trans) rots
          f ((i,j),m) = ((i,j), (compact 2 .s.h) (g i j m))
          g i j m = r i!"ai" * t m * r j!"bj"
          t m = (!"kij").listArray[rows m,3,3].toList $ flatten m
          h x = reshape 9 $ coords (x~>"kab")
          s m = m <> trans sel
          sel = (3><9) [ 0,  0, 0, 0, 0, -1,  0, 1, 0,
                         0,  0, 1, 0, 0,  0, -1, 0, 0,
                         0, -1, 0, 1, 0,  0,  0, 0, 0 ]

----------------------------------------------------------------------

selectUsefulPoints :: Int -> Projections Calibrated -> [Proj]
selectUsefulPoints n p = obs where
    obs = [((r i, j),pt) | ((i,j),pt) <- projs p, i `elem` pts ]
    sel = take n . sortBy (compare `on` negate.length.v_of_p p) . p_of_v p   
    pts = unionSort $ map sel (rangeCams p)
    r i = fromJust $ lookup i assoc
    assoc = zip pts [0..]

estimatePointsCenters :: Int -> [Mat] -> Projections Calibrated -> (Motion,Motion)
estimatePointsCenters n rots p = (newCams sol, newCams (-sol)) where
    irk = arrayOf $ map trans rots
    obs = selectUsefulPoints n p
    coefs = fromBlocks $ map (return . ec) obs
    pmax = maximum (map (fst.fst) obs)
    cmax = nCam p - 1
    ec ((i,j),pix) = fromBlocks [ replicate i z
                               ++ [x]
                               ++ replicate (pmax-i+j) z
                               ++ [-x]
                               ++ replicate (cmax-j) z
                                ]
        where [a,b,c] = toList $ irk j <> hv pix
              x = (3><3) [0,-c,b
                          ,c,0,-a
                          ,-b,a,0]
              z = (3><3) [0,0 .. ] :: Mat

    sol = reshape 3 $ fst $ homogSolve $ debug "systemPC" inforank $ dropColumns 3 coefs
      where inforank m = (rows m, cols m)
    newcens s = toRows $ dropRows (pmax) s
    newCams s = zipWith f rots (newcens s) where f r c = r & asColumn (-r <> c)
    hv (Point x y) = unitary (vec [x,y,1])

