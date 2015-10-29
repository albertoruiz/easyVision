{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Vision.Tensorial(
    fundamental, trifocal, quadrifocal,
    multiView, fourViews, threeViews, twoViews, extractFirst,
    flipDir, refine, relocate, alignPtsTo,
    kal, autoCalibrate, calibrate,
    quality, signalLevel, signalNoise, objectQuality, poseQuality, autoMetric,
    homogT, inhomogT, eps3, eps4, unitT, frobT, normInfT,
    camerasFromTrifocalHZ, correctFundamental,
    getFK, getFs, fixCal, metricConsis,
    rangecoord, normalizeCoords,
    module Vision.TensorRep
)where

import Numeric.LinearAlgebra.Exterior
import Numeric.LinearAlgebra.HMatrix as LA hiding((!),vector,size)
import Numeric.LinearAlgebra.Array.Util as Array
import Numeric.LinearAlgebra.Array.Solve
import System.Random
import Util.Homogeneous(normat)
import Vision.Camera
import Vision.Stereo(depthOfPoint)
import Data.List
import Data.Function(on)
import Control.Applicative
import Control.Monad hiding (join)
import Vision.TensorRep
import Util.Misc(splitEvery,pairsWith)
import Util.Statistics(mean)
import Util.Debug(debug)
import Util.Estimation(procrustes)
import Util.Geometry(homog,inhomog)


-- Homogeneous and inhomogenous versions of a tensor in a given index:

inhomogT :: Name -> Tensor Double -> Tensor Double
inhomogT = flip inhomog

homogT :: Name -> Tensor Double -> Tensor Double
homogT = flip homog

-- Comparation of homogeneous transformations with the same scale:

unitT :: Tensor Double -> Tensor Double
unitT t = t / Array.scalar (frobT t)

frobT :: Tensor Double -> Double
frobT = norm_2 . coords

normInfT :: Tensor Double -> Double
normInfT t = norm_Inf . coords $ t


-- Very frequently used tensors:

eps3 :: Tensor Double
eps3 = cov     (leviCivita 3)

eps4 :: Tensor Double
eps4 = contrav (leviCivita 4)

-- The multiview tensors:

-- We assume in all cases that the image index (dim 3) is lower than the world index (dim 4).
-- It is ok when we rename parts of a problem, we get "1x" etc.

fundamental :: Tensor Double -> Tensor Double -> Tensor Double
fundamental m n = unitT $
  eps3!"rpq" * m!"pa" * m!"qb" * eps4!"abcd" *
  n!"sc" * n!"td" * eps3!"stk"

trifocal :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
trifocal m n p = unitT $
  eps4!"abcd" * m!"ia" * n!"jb" *p!"pc" * p!"qd" * eps3!"pqk"

quadrifocal :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
quadrifocal a b c d = unitT $
  eps4!"ijlk" * a!"pi" * b!"qj" * c!"rk" * d!"sl"




correctFundamental :: Matrix Double -> Matrix Double
correctFundamental f = f' where
   (u,s,v) = svd f
   s1:s2:_ = toList s
   f' = u <> diag (fromList [s1,s2,0.0]) <> tr v


-- The algorithm in Hartley and Zisserman
camerasFromTrifocalHZ :: Tensor Double -> (Tensor Double, Tensor Double, Tensor Double)
camerasFromTrifocalHZ tri = (t1,t2,t3) where
    p3 = cameraAtOrigin
    e1 = unitT $ epitri tri
    e2 = unitT $ epitri (tri!>"jik") -- hmmm!!!
    p1 = (asMatrix $ tri!>"ijk"* cov e2!"j") & asColumn (coords e1)    -- (e in l slot, in HZ)
    p2 = (asMatrix (e2!"i"*e2!"j") - ident 3)
          <> (asMatrix $ tri!>"ijk"* cov e1!"i") & asColumn (coords e2)
    f = unitT . fromMatrix Contra Co
    t1 = f p1 !"1x"
    t2 = f p2 !"2x"
    t3 = f p3 !"3x"
    a & b = fromBlocks[[a,b]]

-- image on c1 of the center of c3
-- This method should only be used on geometrically correct trifocal tensors
epitri :: Tensor Double -> Tensor Double
epitri tri = epi where
     epi = lin tri (vector[0,0,1]) !"a"
           * lin tri (vector[1,0,0])!"b" * (contrav eps3)!"abc"
     lin t p = linQuad t * p!"k" * p!"K"
     linQuad t = (t!>"ijk") * vector[0,0,1]!"a"
                            * eps3!"ajJ" * (t!>"IJK") * eps3!"iIr"


--------------------------------------------------------------------------


--fixRot m = p where
--   (_,r,c) = factorizeCamera m
--   p = r <|> -r<>c

kal :: Tensor Double -> Tensor Double
kal cs = mapTat ((!>"xw").applyAsMatrix getk) ["c"] cs
    where getk m = k where (k,_,_) = factorizeCamera m

-- ks = kal . cam

quality :: VProb -> Double
quality prob = sqrt $ frobT (inhomogT "v" (p2d prob) - inhomogT "v" proj) ^(2::Int) / (n*m*2)
    where
        proj = cam prob * p3d prob
        n = fromIntegral $ size "n" (p2d prob)
        m = fromIntegral $ size "c" (p2d prob)


calibrate :: VProb -> VProb
calibrate p = p { cam = newcams!>"wv", p2d = newp2d!>"wv" } where
    ks = parts (kal $ cam p) "c"
    iks = map ((!"wv").applyAsMatrix inv) ks
    cs = parts (cam p) "c"
    ps = parts (p2d p) "c"
    newcams = subindex "c" $ zipWith (*) iks cs
    newp2d  = subindex "c" $ zipWith (*) iks ps

------------------------------------------------------------------------


multiView :: Int -- ^ 2, 3, or 4 bootstraping views
          -> ALSParam Variant Double
          -> Seed
          -> Tensor Double -- ^ views c v n
          -> VProb
multiView k param seed vs =
    VProb { cam = allcams, p3d = allp3d, p2d = vs,
            l3d= undefined, l2d= undefined } where
    vs2 = (take k `onIndex` "c") vs
    views = renameParts "c" vs2 "v" ""
    dat = outers views

    sol = extractCams param seed dat

    cs = subindex "c" (map (!"vx") sol)
    ps = solveP cs vs2 "v"
    allcams = solveP ps vs "v"
    allp3d  = ps --solveP allcams vs "v"


extractCams :: ALSParam Variant Double
            -> Seed
            -> Tensor Double -- ^ outer products of the views
            -> [Tensor Double]
extractCams param seed dat | order dat -1 == 2 = [f1,f2]
                           | order dat -1 == 3 = [t1,t2,t3]
                           | order dat -1 == 4 = [q1,q2,q3,q4]
                           | otherwise = error $ "extractCams requires 2, 3, or 4 views"
    where solver = mlSolve param [eps4!"pqrs"] (init4Cams seed)
          fun = solveH dat "12"
          ([f1,_,f2,_],_errf) = solver (fun !>"1y 2x" * contrav eps3!"12y" * contrav eps3!"34x")
          tri = solveH (dat * eps3!"1pq" * eps3!"2rs") "pr3" !> "p1 r2 3x"
          ([t1,t2,t3,_],_errt) = solver (tri* contrav eps3!"34x")
          qua = solveH (dat * eps3!"1ab" * eps3!"2fg" * eps3!"3pq" * eps3!"4uv") "afpu"
          ([q1,q2,q3,q4],_errq) = solver (qua!"1234")

init4Cams :: Seed -> [Tensor Double]
init4Cams seed = [ic1!"1p",ic2!"2q",ic3!"3r",ic4!"4s"]
     where [ic1,ic2,ic3,ic4] = parts (randomTensor seed [-4,3,-4]) "1"



-----------------------------------------------------------------------------------

-- | Linear autocalibration for diag(f,f,1) based on the absolute dual quadric.
autoCalibrate :: Maybe Double -> VProb -> VProb
autoCalibrate Nothing  = autoCalibrateAux sel
autoCalibrate (Just f) = autoCalibrateAux (sel3 f)

-- linear autocalibration for diag f1 f2 1 based on the absolute dual quadric
autoCalibrateAux :: Tensor Double -> VProb -> VProb
autoCalibrateAux t p = p { p3d = (p3d p * hi)!>"yx", cam = cs' !> "yx"}
 where
   cs = cam p
   cs' = cs * h
   hi = applyAsMatrix inv h !"yx"
   a = (map (\m-> m * m!"uy") `onIndex` "c") cs
   q = solveH (a*t) "xy"
   h = setType "y" Co $ applyAsMatrix psqrt q


psqrt :: Matrix Double -> Matrix Double
psqrt m | defpos = v <> diag (sqrt s')
        | defneg = psqrt (-m)
        | otherwise = debug "Warning: NODEFPOS eig = " (const s') $ 
                      ident 4

       where (s,v) = eigSH' m
             s' = fromList $ init (toList s) ++ [1]
             [s1,s2,s3,s4] = toList s
             defpos = s3>0 && abs s3 > abs s4
             defneg = s2<0 && abs s2 > abs s1


sel3 :: Double -> Tensor Double
sel3 f = (!"ruv") . cov . mkAssoc [8,3,3] . (dg++) . map (flip (,) 1) $
       [ [0,0,1], [1,0,2], [2,1,2],
         [3,1,0], [4,2,0], [5,2,1] ]
  where dg = [([6,0,0],2),([6,1,1],-2),
              ([7,0,0],2),([7,2,2],-2*f*f)]

sel :: Tensor Double
sel = (!"ruv") . cov . mkAssoc [7,3,3] . (dg++) . map (flip (,) 1) $
      [ [0,0,1], [1,0,2], [2,1,2],
        [3,1,0], [4,2,0], [5,2,1] ]
  where dg = [([6,0,0],2),([6,1,1],-2)]

-------------------------------------------------------------------------

-- | From the views of a 'VProb' (with 3 ór 4 views), solve for the
-- first camera given the other 
extractFirst
    :: Tensor Double -- ^ views c v n
    -> [Matrix Double] -- ^ previous known cams (2 or 3)
    -> Matrix Double   -- ^ the first camera
extractFirst vs pcams = newcam where
    vs2 = (take 4 `onIndex` "c") vs
    views = renameParts "c" vs2 "v" ""
    dat = outers views
    newcam = extract1 pcams dat


extract1
    :: [Matrix Double]
    -> Tensor Double -- ^ outer products of the views
    -> Matrix Double

extract1 [c2,c3] dat = normat $ asMatrix sol where
    tri = solveH (dat * eps3!"1pq" * eps3!"2rs") "pr3" !> "p1 r2 3x"
    tc2 = fromMatrix Contra Co c2 !"2q"
    tc3 = fromMatrix Contra Co c3 !"3r"
    tc4 = fromMatrix Contra Co c3 !"4s"
    sol = solve (eps4!"pqrs"*tc2*tc3*tc4) (tri* contrav eps3!"34x")

extract1 [c2,c3,c4] dat = normat $ asMatrix sol where
    qua = solveH (dat * eps3!"1ab" * eps3!"2fg" * eps3!"3pq" * eps3!"4uv") "afpu"
    tc2 = fromMatrix Contra Co c2 !"2q"
    tc3 = fromMatrix Contra Co c3 !"3r"
    tc4 = fromMatrix Contra Co c4 !"4s"
    sol = solve (eps4!"pqrs"*tc2*tc3*tc4) (qua!"1234")

extract1 _ _ = error "extractFirst requires 3 or 4 views"

---------------------------------------------------------------------------

-- | Make sure that the cameras look at the points
flipDir :: VProb -> VProb
flipDir p = p { p3d = (p3d p * hi)!>"yx", cam = (cam p * h) !> "yx"} where
    hi = applyAsMatrix inv h !"yx"
    h = listTensor [4,-4] [1,0,0,0,
                           0,1,0,0,
                           0,0,1,0,
                           0,0,0,d] !"xy"
    c1 = asMatrix $ head $ parts (cam p) "c"
    p1 = head $ ihPoints3D p
    d = signum $ depthOfPoint (toList p1) c1

------------------------------------------------------------------

relocate :: VProb -> VProb
relocate p = p { p3d = (p3d p * hi)!>"yx", cam = (cam p * h) !> "yx"} where
    hi = applyAsMatrix inv h !"yx"
    h = listTensor [4,-4] [s,0,0,x,
                           0,s,0,y,
                           0,0,s,z,
                           0,0,0, 1] !"xy"
    cens = cameraCenters p
    pts = ihPoints3D p
    (m,c) = meanCov . fromRows $ (concat $ replicate (length cens) pts) ++ (concat $ replicate (length pts) cens)
    [x,y,z] = toList m
    s = sqrt $ maxElement $ eigenvaluesSH c 

-------------------------------------------------

alignPtsTo :: VProb -> VProb -> VProb
-- ^ metric alignment of two reconstructions for visual inspection
--   (we check a possible mirror image solution)
alignPtsTo p q = if e1 < e2 then q1 else q2
  where
    (e1,q1) = alignPts' (-1) p q
    (e2,q2) = alignPts'   1  p q

    alignPts' sg p q = (err, q {p3d = okp3d, cam = okCams})
      where
        di = diagl [sg,1,1]
        a = asMatrix $ inhomogT "x" $ p3d q
        b = asMatrix $ inhomogT "x" $ p3d p
        (s,r,d) = procrustes b (a <> tr di)
        m = diagl [s,s,s,1]
            <>
            fromBlocks [[r,asColumn d],
                        [0,1]]
            <> diagl [sg,1,1,1]
        t = (transf $ toLists m) !"yx"
        it = (transf $ toLists $ inv m) !"xy"
        okp3d = (p3d q * t) !"nx"
        okCams = (cam q * it) !"cvx"
        err = norm_Frob (a - LA.scalar s * (b <> tr r + asRow d))

--------------------------------------------------------------------------------


ihPoints3D :: VProb -> [Vector Double]
ihPoints3D = map coords . flip parts "n" . inhomogT "x" . p3d

--dstmat :: (Normed b, Num b) => [b] -> [Double]
dstmat pts = pairsWith dist pts where
  dist u v = norm_2 (u-v)

--metricConsis :: (Normed b, Num b) => (a -> [b]) -> a -> a -> Double
metricConsis f = on dist (nor. dstmat .f) where
  nor x = fromList x / LA.scalar (mean x)
  dist u v = norm_Inf (u-v)

objectQuality :: VProb -> VProb -> Double
objectQuality = metricConsis ihPoints3D

cameraCenters :: VProb -> [Vector Double]
cameraCenters = map (coords . inhomogT "x" . flip solveH "x") . flip parts "c" . cam

poseQuality :: VProb -> VProb -> Double
poseQuality = metricConsis cameraCenters

paramEq :: ALSParam Variant Double
paramEq = defaultParameters {post = eqnorm}

refine :: VProb -> VProb
refine p = p { p3d = newpoints, cam = newcams }
    where ([newcams,newpoints], _err) = mlSolveP paramEq [1] [cam p, p3d p] (p2d p) "v"

fourViews :: Seed -> Tensor Double -> VProb
fourViews  = multiView 4 paramEq

threeViews :: Seed -> Tensor Double -> VProb
threeViews = multiView 3 paramEq

twoViews :: Seed -> Tensor Double -> VProb
twoViews   = multiView 2 paramEq

-- from kal . cam
getFs :: Tensor Double -> [Double]
getFs = map (getFK . asMatrix) . flip parts "c"

getFK :: Matrix Double -> Double
getFK = (/2) . sum . toList . abs . subVector 0 2 . takeDiag

fixCal :: Tensor Double -> Tensor Double
fixCal = subindex "c" . map ((!>"1v 2w") . fromMatrix Contra Co . kgen) . getFs

autoMetric :: VProb -> Double
autoMetric p = frobT (k - fixCal k) / fromIntegral (5 * size "c" k)
     where k = kal . cam $ p

rangecoord :: VProb -> ((Double, Double), (Double, Double))
rangecoord p = ((minElement x, maxElement x),(minElement y, maxElement y)) where
    [x,y] = toRows $  fibers "v" (inhomogT "v" $ p2d p)



normalizeCoords :: (Int, Int) -> VProb -> VProb
normalizeCoords sz p = p {p2d = p2d p!>"vw" * fromMatrix Contra Co (inv $ knor sz) !"vw"}

signalLevel :: VProb -> Double
signalLevel = mean . map (sqrt . minElement . eigenvaluesSH . snd . meanCov . asMatrix . (~>"nv")) .  flip parts "c" . inhomogT "v" . p2d

signalNoise :: VProb -> Double
signalNoise p = (quality p / signalLevel p)

getCams p = map asMatrix $ parts (cam p) "c"

getP3d p = toRows $ asMatrix $ p3d p

