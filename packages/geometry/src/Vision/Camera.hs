{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.Camera
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Projective camera synthesis and analysis

-}
-----------------------------------------------------------------------------

module Vision.Camera
( CameraParameters(..)
, syntheticCamera
, easyCamera
, cameraAtOrigin
, factorizeCamera
, rotOfCam
, mkCamera
, sepCam
, poseFromFactorization
, poseFromCamera
, homogZ0
, focalFromHomogZ0
, cameraFromHomogZ0
, cameraFromAffineHomogZ0
, poseFromHomogZ0
, cameraFromPlane
, kgen
, knor
, cameraOutline
, drawCameras
, toCameraSystem
, estimateCamera
, estimateCameraRaw
, computeCamera, computeCameraRaw
, computeHomography, computeHomographyRaw
, linearPose, computeLinearPose, computeLinearPosePlanar
, rectifierFromCircularPoint
, rectifierFromAbsoluteDualConic
, estimateAbsoluteDualConic
, focalFromCircularPoint
, imagOfCircPt, getHorizs, selectSol
, circularConsistency
, cameraModelOrigin
, projectionAt, projectionAtF
, projectionDerivAt, projectionDerivAtF
, epipolarMiniJac
, projectionDerivAt'
, projectionDerivAtH
, refineNewton
, auxCamJac, auxCamJacK
, projectionAt'', projectionAt'
, theRightB, theRight
) where

import Util.Camera

import Numeric.LinearAlgebra.Compat as LA
import qualified Numeric.GSL as G
import Util.Homogeneous
import Util.Estimation(homogSolve, withNormalization, withNormalization', estimateHomography,procrustes)
import Util.Rotation
--import Data.List(transpose,nub,maximumBy,genericLength,elemIndex, genericTake, sort)
--import System.Random
import Graphics.Plot(gnuplotWin)

import Util.Misc(mat,vec,Mat,Vec,degree,impossible,median)
import Util.Ellipses(intersectionEllipses,InfoEllipse(..))

import Util.Geometry
import Util.Small(unsafeMap)
import Numeric.LinearAlgebra.Util((¦),(#),row,norm,unitary,diagl)
import Control.Arrow

-- | A nice camera parameterization.
data CameraParameters 
    = CamPar { focalDist                      :: Double
             , panAngle, tiltAngle, rollAngle :: Double
             , cameraCenter                   :: (Double,Double,Double)
             } deriving Show


-- | Computes the camera parameters given the projection center,
--   the point imaged in the image center and roll angle.
easyCamera :: Double                   -- ^ field of view
           -> (Double,Double,Double)   -- ^ camera center
           -> (Double,Double,Double)   -- ^ a point in front of the camera
           -> Double                   -- ^ roll angle
           -> CameraParameters
easyCamera fov cen@(cx,cy,cz) (x,y,z) rho  = 
    CamPar { focalDist = f
           , panAngle = beta
           , tiltAngle = alpha
           , rollAngle = rho
           , cameraCenter = cen
           } where 
    dx = x-cx
    dy = y-cy
    dz = z-cz
    dh = sqrt (dx*dx+dy*dy)
    f = 1 / tan (fov/2)
    beta = atan2 (-dx) dy
    alpha = atan2 dh (-dz) 

-- | Obtains the 3x4 homogeneous transformation from world points to image points.
syntheticCamera :: CameraParameters -> Mat
syntheticCamera campar = flipx <> k <> r <> m where
    CamPar {focalDist = f, 
            panAngle = p, tiltAngle = t, rollAngle = q,
            cameraCenter = (cx,cy,cz)} = campar
    m = mat [[1,0,0, -cx],
             [0,1,0, -cy],
             [0,0,1, -cz]]
    r = rotPTR (p,t,q)
    k = kgen f


rotPTR :: (Double,Double,Double) -> Mat
rotPTR (pan,tilt,roll) = (3><3)
   [ -cb*cg + ca*sb*sg, -cg*sb - ca*cb*sg, -sa*sg 
   ,   ca*cg*sb + cb*sg, -ca*cb*cg + sb*sg, -cg*sa 
   ,              sa*sb,            -cb*sa,     ca ]
   where cb = cos pan 
         sb = sin pan 
         ca = cos tilt
         sa = sin tilt
         cg = cos roll
         sg = sin roll

{-
focal' :: Mat -> Maybe Double
focal' c = res where
    n = c <> mS <> trans c <> linf
    d = c <> mA <> trans c <> linf
    x = c <> mF <> trans c <> linf
    xi = inHomog x
    ni = inHomog n
    f = sqrt $ norm (xi - ni) ^2 - norm ni ^2
    res = if f > 0 then Just f else Nothing 
-}

-- | Tries to compute the focal dist of a camera given the homography Z0 -> image
focalFromHomogZ0 :: Mat -> Maybe Double
focalFromHomogZ0 c = res where
    [a11,a12,_, 
     a21,a22,_, 
     a31,a32,_] = toList (flatten c)
    nix = (a11*a31 + a12 *a32)/den 
    niy = (a21*a31 + a22 *a32)/den
    xix = (a12 *(-a31 + a32) + a11 *(a31 + a32))/den
    xiy = (a22 *(-a31 + a32) + a21 *(a31 + a32))/den
    den = a31**2 + a32**2
    f = sqrt $ (xix-nix)**2 +(xiy-niy)**2 - nix**2 - niy**2
    res = if f > 0 then Just f else Nothing

-- | Obtains the pose of a factorized camera. (To do: check that is not in the floor).
poseFromFactorization :: (Mat,Mat,Vec)  -- ^ (k,r,c) as obtained by factorizeCamera
                      -> CameraParameters
poseFromFactorization (k,r,c) = cp where
    cp = CamPar {focalDist = f,
                 panAngle = -beta,
                 tiltAngle = alpha,
                 rollAngle = -rho,
                 cameraCenter = (cx, cy, cz) }
    f = (k@@>(0,0)+k@@>(1,1))/2 -- hmm
    [cx,cy,cz] = toList c
    [r1,r2,_] = toColumns r
    h = fromColumns [r1,r2,-r<>c]
    b = trans h <> linf
    beta = atan2 (b@>0) (b@>1)
    n = k <> h <> mS <> b
    ni = inHomog n
    rho = atan2 (ni@>0) (ni@>1)
    alpha = atan2 f (norm ni)

-- | Extracts the camera parameters of a diag(f,f,1) camera
poseFromCamera :: Mat              -- ^ 3x4 camera matrix
               -> CameraParameters
poseFromCamera = poseFromFactorization . factorizeCamera

-- | Tries to extract the pose of the camera from the homography of the floor
poseFromHomogZ0 :: Maybe Double      -- ^ focal distance (if known)
                -> Mat                    -- ^ 3x3 floor to image homography
                -> Maybe CameraParameters      -- ^ solution (the one above the floor)
poseFromHomogZ0 mbf = fmap poseFromCamera . cameraFromHomogZ0 mbf


-- | Obtains the homography floor (Z=0) to image from a camera
homogZ0 :: Mat -> Mat
homogZ0 cam = extractColumns [0,1,3] cam

-- | Recovers a camera matrix from the homography floor (Z=0) to image. There are actually two solutions, above and below the ground. We return the camera over the floor
cameraFromHomogZ0 :: Maybe Double              -- ^ focal distance (if known)
           -> Mat                          -- ^ 3x3 floor to image homography
           -> Maybe Mat                 -- ^ 3x4 camera matrix (solution over the floor)
cameraFromHomogZ0 mbf c = res where
    mf = case mbf of
            Nothing -> focalFromHomogZ0 c     -- unknown, we try to estimate it
            jf -> jf                          -- given, use it
    res = case mf of
            Just _ -> Just m      -- solution
            Nothing -> Nothing    -- cannot be estimated
    Just f = mf
    s = kgen (1/f) <> c
    [s1,s2,s3] = toColumns s
    sc = norm s1
    t = s3 / scalar sc
    r1 = unitary s1
    r3 = unitary (cross s1 s2)
    r2 = cross r3 r1
    rot = fromColumns [r1,r2,r3]
    cen1 = - (trans rot <> t)
    m1 = kgen f <> fromColumns [ r1, r2,r3, t]
    m2 = kgen f <> fromColumns [-r1,-r2,r3,-t]
    m = if cen1@>2 > 0 then m1 else m2

----------------------------------------------------------------------

-- | computes an affine camera from an affine homography of the floor plane.
--   It requires a non optional focal distance.
cameraFromAffineHomogZ0 :: Double -> Mat -> Mat
cameraFromAffineHomogZ0 f h = cam
  where
    h0 = diagl [1,1,f] <> h
    s = sqrt(h0 @@>(0,0) **2 + h0 @@>(0,1) **2)
    h1 = h0 / scalar s
    [ [r1, r2, tx]
     ,[r3, r4, ty]
     ,[ _,  _, tz]] = toLists h1
    
    cam = diagl[f,f,1] <> (3><4) [ r1, r2, x, tx,
                                   r3, r4, y, ty,
                                   0 ,  0, 0, tz ]
    
    x =    - (sqrt(2)*
         (r1*r3 + r2*r4))/
       sqrt(r1**2 + r2**2 - 
         r3**2 - r4**2 + 
         sqrt(((r2 + r3)**
              2 + 
             (r1 - r4)**2)*
           ((r2 - r3)**2 + 
             (r1 + r4)**2)))
             
    y =    (sqrt(r1**2 + 
           r2**2 - r3**2 - 
           r4**2 +
           sqrt(((r2 + r3)**
               2 + 
               (r1 - r4)**2)*
             ((r2 - r3)**2 + 
               (r1 + r4)**2))
           )/sqrt(2))    

----------------------------------------------------------------------


cameraOutline :: Double -> [[Double]]
cameraOutline f =
    [
    [0::Double,0,0],
    [1,0,0],
    [0,0,0],
    [0,0.75,0],
    [0,0,0],
    [-1,1,f],
    [1,1,f],
    [1,-1,f],
    [-1,-1,f],
    [-1,1,f],
    [0,0,0],
    [1,1,f],
    [0,0,0],
    [-1,-1,f],
    [0,0,0],
    [1,-1,f],
    [0,0,0]
    --,[0,0,3*f]
    ]


{-
doublePerp (a,b) (c,d) = (e,f) where
    a' = vec a
    b' = vec b
    c' = vec c
    d' = vec d
    v = cross (b'-a') (d'-c')
    coef = fromColumns [b'-a', v, c'-d']
    term = c'-a'
    [lam,mu,ep] = toList (inv coef <> term)
    e = toList $ a' + scalar lam * (b'-a')
    f = toList $ a' + scalar lam * (b'-a') + scalar mu * v
-}
------------------------------------------------------            

-- | linear camera resection
estimateCameraRaw :: [[Double]] -> [[Double]] -> Mat
estimateCameraRaw image world = h where
    eqs = concat (zipWith eq image world)
    h = reshape 4 $ fst $ homogSolve (mat eqs)
    eq [bx,by] [ax,ay,az] = 
        [[  0,  0,  0,  0,t15,t16,t17,t18,t19,t110,t111,t112],
         [t21,t22,t23,t24,  0,  0,  0,  0,t29,t210,t211,t212],
         [t31,t32,t33,t34,t35,t36,t37,t38,  0,   0,   0,   0]] where
            t15 =(-ax)
            t16 =(-ay)
            t17 =(-az)
            t18 =(-1)
            t19 =by*ax 
            t110=by*ay 
            t111=by*az
            t112=by
            t21 =ax 
            t22 =ay 
            t23 =az
            t24 =1
            t29 =(-bx*ax) 
            t210=(-bx*ay)
            t211=(-bx*az) 
            t212=(-bx)
            t31=(-by*ax) 
            t32=(-by*ay) 
            t33=(-by*az)
            t34=(-by)
            t35=bx*ax 
            t36=bx*ay
            t37=bx*az
            t38=bx
    eq _ _ = impossible "eq in estimateCameraRaw"                  

estimateCamera :: [[Double]] -> [[Double]] -> Mat
estimateCamera = withNormalization inv estimateCameraRaw 

--------------------------------------------------------------------------------

computeCamera :: [Point] -> [Point3D] -> Camera
-- ^ linear camera resection with coordinate normalization
computeCamera image world = unsafeFromMatrix $ withNormalization' inv computeCameraRaw image world

computeCameraRaw :: [Point] -> [Point3D] -> Mat
-- ^ linear camera resection
computeCameraRaw image world = c where
    eqs = concat (zipWith eq image world)
    c = reshape 4 $ fst $ homogSolve (mat eqs)
    eq (Point p q) (Point3D x y z) =
        [ [   0,   0,   0, 0, -x, -y, -z,-1, q*x, q*y, q*z, q]
        , [   x,   y,   z, 1,  0,  0,  0, 0,-p*x,-p*y,-p*z,-p]
        , [-q*x,-q*y,-q*z,-q,p*x,p*y,p*z, p,   0,   0,   0, 0] ]

--------------------------------------------------------------------------------

computeHomography :: [Point] -- ^ dst
                  -> [Point] -- ^ src
                  -> Homography
-- ^ linear homography estimation with coordinate normalization
computeHomography image world = unsafeFromMatrix $ withNormalization' inv computeHomographyRaw image world

computeHomographyRaw :: [Point] -> [Point] -> Mat
-- ^ linear homography estimation
computeHomographyRaw image world = c where
    eqs = concat (zipWith eq image world)
    c = reshape 3 $ fst $ homogSolve (mat eqs)
    eq (Point p q) (Point x y) =
        [ [   0,    0,  0,  -x,  -y,-1, q*x, q*y, q]
        , [   x,    y,  1,   0,   0, 0,-p*x,-p*y,-p]
        , [-q*x, -q*y, -q, p*x, p*y, p,   0,   0, 0] ]


computeHomographyH :: [HPoint] -- dst
                   -> [HPoint] -- src
                   -> Homography
-- ^ linear homography estimation from homogeneous points
computeHomographyH image world = unsafeFromMatrix c
  where
    c = trans $ reshape 3 $ fst $ homogSolve eqs
    eqs = fromBlocks (zipWith eq image world)
    eq (toVector -> y) (toVector -> x) = [ asRow x `kronecker` asMat y ]
-- Fusiello style

--------------------------------------------------------------------------------

-- | Estimation of a camera matrix from image - world correspondences, for world points in the plane z=0. We start from the closed-form solution given by 'estimateHomography' and 'cameraFromHomogZ0', and then optimize the camera parameters by minimization (using the Nelder-Mead simplex algorithm) of reprojection error.
cameraFromPlane :: Double        -- ^ desired precision in the solution (e.g., 1e-3)
                -> Int           -- ^ maximum number of iterations (e.g., 300)
                -> Maybe Double  -- ^ focal dist, if known
                -> [[Double]]    -- ^ image points as [x,y]
                -> [[Double]]    -- ^ world points in plane z=0, as [x,y]
                -> Maybe (Mat, Mat)  -- ^ 3x4 camera matrix and optimization path
cameraFromPlane prec nmax mbf image world = camera where
    h = estimateHomography image world
    camera = case cameraFromHomogZ0 mbf h of
        Nothing -> Nothing
        Just p  -> Just $ refine p
                     where refine = case mbf of Nothing -> refineCamera1
                                                _       -> refineCamera2
    mview = fromLists image
    mworld = fromLists (map pl0 world)
    pl0 [x,y] = [x,y,0]
    pl0 _ = impossible "pl0 in cameraFromPlane"


    refineCamera1 cam = (betterCam,path) where
        initsol = par2list $ poseFromCamera cam
        (betterpar, path) = minimize cost initsol
        betterCam = syntheticCamera $ list2par betterpar
        cost lpar = pnorm Frobenius (mview - htm c mworld)
            where c = syntheticCamera $ list2par lpar
        minimize f xi = G.minimize G.NMSimplex2 prec nmax  [0.01,5*degree,5*degree,5*degree,0.1,0.1,0.1] f xi

    refineCamera2 cam = (betterCam,path) where
        f:initsol = par2list $ poseFromCamera cam
        (betterpar, path) = minimize cost initsol
        betterCam = syntheticCamera $ list2par (f:betterpar)
        cost lpar = {-# SCC "cost2" #-} pnorm Frobenius (mview - htm c mworld)
            where c = syntheticCamera $ list2par (f:lpar)
        minimize f' xi = G.minimize G.NMSimplex2 prec nmax [5*degree,5*degree,5*degree,0.1,0.1,0.1] f' xi


    list2par [f,p,t,r,cx,cy,cz] = CamPar f p t r (cx,cy,cz)
    list2par _ = impossible "list2par in cameraFromPlane"

    par2list (CamPar f p t r (cx,cy,cz)) = [f,p,t,r,cx,cy,cz]

--------------------------------------------------------------------------------

linearPose :: [Point] -> [Point3D] -> Camera
-- ^ Fiore's method
linearPose image world = unsafeFromMatrix (r ¦ asColumn t)
  where
    x = fromRows (map toVector world)
    m = trans x # 1
    v = fromColumns $ nullspacePrec 1 m
    p = trans (fromRows $ map toVector image) # 1
    d = diagBlock $ map asColumn $ toColumns p
    vk = kronecker (trans v) (ident 3)
    a = vk <> d
    depths = fst $ homogSolve a
    sgn = signum $ median $ toList depths
    pd = asRow depths * p * scalar sgn
    (_,r,t) = procrustes (trans pd) x


computeLinearPose :: Double -> [Point] -> [Point3D] -> Camera
-- ^ 'linearPose' with calibration diag(f,f,1)
computeLinearPose f image world = k ⊙ m
  where
    m = linearPose image' world
    image' = invTrans k ◁ image
    k = unsafeFromMatrix (kgen f) :: Homography


computeLinearPosePlanar :: Double -> [Point] -> [Point] -> Homography
-- ^ 'computeLinearPose' from a planar reference
computeLinearPosePlanar f image world = h
  where
    g (Point x y) = Point3D x y 0
    c = computeLinearPose f image (map g world)
    [c1,c2,_,c4] = toColumns $ toMatrix c
    h = unsafeFromMatrix $ fromColumns [c1,c2,c4]

--------------------------------------------------------------------------------

-- Metric rectification tools

rectifierFromCircularPoint :: (Complex Double, Complex Double) -> Mat
rectifierFromCircularPoint (x,y) = rectifierFromAbsoluteDualConic omega where
    cir = fromList [x,y,1]
    omega = fst $ fromComplex $ cir `outer` conj cir + conj cir `outer` cir

rectifierFromAbsoluteDualConic :: Mat -> Mat
rectifierFromAbsoluteDualConic omega = inv t where
    (_,s,u) = svd omega
    [s1,s2,_] = toList s
    s' = fromList [s1,s2,1]
    t = u <> diag (sqrt s')
    -- 0 =~= norm $ (normat3 $ t <> diagl [1,1,0] <> trans t) - (normat3 omega)

-- | from pairs of images of orthogonal lines
estimateAbsoluteDualConic ::  [([Double],[Double])] -> Maybe Mat
estimateAbsoluteDualConic pls = clean where
    con = (3><3) [a,c,d
                 ,c,b,e
                 ,d,e,f]
    [a,b,c,d,e,f] = toList $ fst $ homogSolve $ mat eqs
    eqs = map eq pls
    eq ([a,b,c],[a',b',c']) = [a*a', b*b', a*b'+a'*b, c*a'+a*c', c*b'+c'*b, c*c']
    eq (_,_) = impossible "eq in estimateAbsoluteDualConic"
    (l,v) = eigSH' con
    [l1,l2,l3] = toList l
    ok = length pls >= 5 && (l1>0 && l2>0 || l2<0 && l3<0)
    di = if l2>0 then diagl [l1,l2,0] else diagl [0,-l2,-l3]
    clean | ok        = Just $ v <> di <> trans v
          | otherwise = Nothing

focalFromCircularPoint :: (Complex Double,Complex Double) -> Double
focalFromCircularPoint (cx,cy) = x * sqrt (1-(y/x)**2) where
    j' = fromList [cx,cy]
    pn = fst $ fromComplex j'
    x = pnorm PNorm2 (complex pn - j')
    y = norm pn
    -- alpha = asin (y/x)

-- | Consistency with diag(f,f,1) camera.
circularConsistency :: (Complex Double, Complex Double) -> Double
circularConsistency (x,y) = innerLines n0 h where
    n0 = fromList[realPart x, realPart y, 1] `cross` fromList[0,0,1]
    h = snd $ fromComplex $ cross jh (conj jh)
    jh = fromList [x,y,1]

    innerLines l m = (l.*.m)/ sqrt (l.*.l) / sqrt(m.*.m)
        where a.*.b = a <.> mS <.> b

imagOfCircPt :: InfoEllipse -> InfoEllipse -> Maybe (Complex Double,Complex Double)
imagOfCircPt e1 e2 = fst (selectSol m1 m2 (intersectionEllipses c1 c2))
  where
    [m1,m2] = map conicCenter [e1,e2]
    [c1,c2] = map conicMatrix [e1,e2]

getHorizs pts = map linf pts where
    linf (x,y) = toList $ unitary $ snd $ fromComplex $ cross v (conj v)
        where v = fromList [x,y,1]

selectSol (x1,y1) (x2,y2) pts = (ij,other) where
    ls = getHorizs pts
    ij    = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c
    mbhead [] = Nothing
    mbhead x  = Just (head x)

--------------------------------------------------------------------------------
-- camera parameterization and Jacobian

type CameraParts = (Mat, Mat, Double, Double, Double)

cameraModelOrigin :: Maybe Mat -> Mat -> CameraParts
cameraModelOrigin (Just k0) m = (k0,r0,cx0,cy0,cz0) where
    (_,r0,c) = factorizeCamera m
    [cx0,cy0,cz0] = toList c

cameraModelOrigin Nothing m = (k0,r0,cx0,cy0,cz0) where
    (k,r0,c) = factorizeCamera m
    [f1,f2,_] = toList (takeDiag k)
    k0 = kgen ((f1+f2)/2)
    [cx0,cy0,cz0] = toList c

projectionAt :: Mat -> Maybe Mat -> [Double] -> Matrix Double
projectionAt m f = \[p,t,r,cx,cy,cz] -> k0 <> rot1 p  <> rot2 t  <> rot3 r  <> r0 <> desp34 (cx0+cx) (cy0+cy) (cz0+cz)
    where (k0,r0,cx0,cy0,cz0) = cameraModelOrigin f m

projectionAtF :: Mat -> Maybe Mat -> [Double] -> Matrix Double
projectionAtF m f = \[g,p,t,r,cx,cy,cz] -> kgen g <> k0 <> rot1 p  <> rot2 t  <> rot3 r  <> r0 <> desp34 (cx0+cx) (cy0+cy) (cz0+cz)
    where (k0,r0,cx0,cy0,cz0) = cameraModelOrigin f m

projectionDerivAt :: Mat -> Mat -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [[Double]]
projectionDerivAt k0 r0 cx0 cy0 cz0 p t r cx cy cz x y z = ms where
    r1 = rot1 p
    r2 = rot2 t
    r3 = rot3 r
    a = k0 <> r1
    b =  a <> r2
    c =  b <> r3
    d =  c <> r0
    e = vec [x-cx0-cx, y-cy0-cy, z-cz0-cz]
    m0 = d <> e
    m4 = d <> vec [-1,0,0]
    m5 = d <> vec [0,-1,0]
    m6 = d <> vec [0,0,-1]
    m7 = -m4
    m8 = -m5
    m9 = -m6
    f = r0 <> e
    m3 = b <> rot3d r <> f
    g = r3 <> f
    m2 = a <> rot2d t <> g
    m1 = k0 <> rot1d p <> r2 <> g
    m0l = toList m0
    ms = iH m0l : map (derIH m0l . toList) [m1,m2,m3,m4,m5,m6,m7,m8,m9]

projectionDerivAtF :: Mat -> Mat -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> [[Double]]
projectionDerivAtF k0 r0 cx0 cy0 cz0 f' p t r cx cy cz x y z = ms where
    r1 = rot1 p
    r2 = rot2 t
    r3 = rot3 r
    u = kgen f' <> k0
    a =  u <> r1
    b =  a <> r2
    c =  b <> r3
    d =  c <> r0
    e = vec [x-cx0-cx, y-cy0-cy, z-cz0-cz]
    m0 = d <> e
    m4 = d <> vec [-1,0,0]
    m5 = d <> vec [0,-1,0]
    m6 = d <> vec [0,0,-1]
    m7 = -m4
    m8 = -m5
    m9 = -m6
    f = r0 <> e
    m3 = b <> rot3d r <> f
    g = r3 <> f
    m2 = a <> rot2d t <> g
    h = r2 <> g
    m1 = u <> rot1d p <> h
    mf = diagl[1,1,0] <> k0 <> r1 <> h
    m0l = toList m0
    ms = iH m0l : map (derIH m0l . toList) [mf,m1,m2,m3,m4,m5,m6,m7,m8,m9]

derIH :: [Double] -> [Double] -> [Double]
derIH [x,y,w] [xd,yd,wd] = [ (xd*w-x*wd)/w**2 , (yd*w-y*wd)/w**2 ]
derIH _ _ = impossible "derIH"

iH :: [Double] -> [Double]
iH [x,y,w] = [x/w,y/w]
iH _ = impossible "iH"

---------------------------------------------------------------

projectionAt'' :: CameraParts -> Vec -> Mat
projectionAt'' (_,r0,cx0,cy0,cz0) = f where
    f (toList -> [p,t,r,cx,cy,cz]) = rot1 p  <> rot2 t  <> rot3 r  <> r0 <> desp34 (cx0+cx) (cy0+cy) (cz0+cz)

projectionAt' :: CameraParts -> Vec -> Mat
projectionAt' (k0,r0,cx0,cy0,cz0) = f where
    f (toList -> [p,t,r,cx,cy,cz]) = k0 <> rot1 p  <> rot2 t  <> rot3 r  <> r0 <> desp34 (cx0+cx) (cy0+cy) (cz0+cz)

type CamJacobianData = (Mat,Mat,Mat,Mat,Vec,Vec,Vec,Double,Double,Double)


auxCamJacK :: CameraParts -> Vec -> CamJacobianData
auxCamJacK (k0,r0,cx0,cy0,cz0) (toList -> [p,t,r,cx,cy,cz]) = (rt,rt1,rt2,rt3,m4,m5,m6,cx+cx0,cy+cy0,cz+cz0) where
    r1 = rot1 p
    r2 = rot2 t
    r3 = rot3 r
    a = k0 <> r1
    b =  a <> r2
    c =  b <> r3
    d =  c <> r0
    rt = d
    [m4,m5,m6] = toColumns (-d)
    rt3 = b <> rot3d r <> r0
    g = r3 <> r0
    rt2 = a <> rot2d t <> g
    rt1 = k0 <> rot1d p <> r2 <> g

auxCamJac :: CameraParts -> Vec -> CamJacobianData
auxCamJac (_,r0,cx0,cy0,cz0) (toList -> [p,t,r,cx,cy,cz]) = (rt,rt1,rt2,rt3,m4,m5,m6,cx+cx0,cy+cy0,cz+cz0) where
    r1 = rot1 p
    r2 = rot2 t
    r3 = rot3 r
    b =  r1 <> r2
    c =  b <> r3
    d =  c <> r0
    rt = d
    [m4,m5,m6] = toColumns (-d)
    rt3 = b <> rot3d r <> r0
    g = r3 <> r0
    rt2 = r1 <> rot2d t <> g
    rt1 = rot1d p <> r2 <> g


projectionDerivAt' :: CamJacobianData -> Vec -> (Vec,Mat,Mat)
projectionDerivAt' (rt,rt1,rt2,rt3,m4,m5,m6,cx,cy,cz) (toList -> [x',y',z']) = result where
    e = fromList [x'-cx, y'-cy, z'-cz]
    m0 = rt <> e
    m1 = rt1 <> e
    m2 = rt2 <> e
    m3 = rt3 <> e
    m7 = -m4
    m8 = -m5
    m9 = -m6
    [x,y,w] = toList m0
    d1 = recip w
    d2 = -x/w**2
    d3 = -y/w**2
    deriv = (2><3) [d1, 0,  d2,
                    0 , d1, d3 ]
    result = (fromList [x/w,y/w],
              deriv <> fromColumns [m1,m2,m3,m4,m5,m6],
              deriv <> fromColumns [m7,m8,m9])

-- for homogeneous point
projectionDerivAtH (rt,rt1,rt2,rt3,m4,m5,m6,cx,cy,cz) pt@(toList -> [x',y',z',w'])  = result where
    e = fromList [x'-w'*cx, y'-w'*cy, z'-w'*cz]
    h = (*scalar w')
    m0 = rt <> e
    m1 = rt1 <> e
    m2 = rt2 <> e
    m3 = rt3 <> e
    [x,y,w] = toList m0
    d1 = recip w
    d2 = -x/w**2
    d3 = -y/w**2
    deriv = (2><3) [d1, 0,  d2,
                    0 , d1, d3 ]
    result = (fromList [x/w,y/w],
              deriv <> fromColumns [m1,m2,m3, h m4, h m5, h m6])




epipolarMiniJac :: CamJacobianData -> CamJacobianData -> (Mat,Mat,Mat)
epipolarMiniJac (r,r1,r2,r3,_,_,_,cx,cy,cz) (q,q1,q2,q3,_,_,_,dx,dy,dz) = result where
    c21 = fromList [dx-cx,dy-cy,dz-cz]
    t = unitary c21
    t1 = derNor c21 (vec [1,0,0])
    t2 = derNor c21 (vec [0,1,0])
    t3 = derNor c21 (vec [0,0,1])

    a = q <> asMat t

    f = a <> trans r

    f1 = a <> trans r1
    f2 = a <> trans r2
    f3 = a <> trans r3

    f10 = q <> asMat t1 <> trans r
    f11 = q <> asMat t2 <> trans r
    f12 = q <> asMat t3 <> trans r
    f4 = -f10
    f5 = -f11
    f6 = -f12

    b = asMat t <> trans r

    f7 = q1 <> b
    f8 = q2 <> b
    f9 = q3 <> b

    g = fromColumns . map (flatten . trans)

    result =  (g [f], g [f1,f2,f3,f4,f5,f6], g [f7,f8,f9,f10,f11,f12])

derNor :: Vec -> Vec -> Vec
derNor v w = scale nv w + scale (-(w <.> v)*vv*nv) v
    where vv = recip (v <.> v)
          nv = sqrt vv

--------------------------------------------------

-- shcam :: Matrix Double -> [[Double]]
-- shcam p = c where
--    (h,f) = toCameraSystem p
--    c = ht (h <> diag (fromList [1,1,1,5])) (cameraOutline f)
-- 
-- drawCameras :: String -> [Matrix Double] -> [[Double]] -> IO ()
-- drawCameras tit ms pts = do
--   let cmd = map (f.shcam) ms
--       f c = (c,"notitle 'c1' with lines 1")
-- 
--   gnuplotpdf tit
--          (  "set view 72,200; "
--          ++ "set xlabel '$x$'; set ylabel '$y$'; set zlabel '$z$';"
--          ++ "set size ratio 1;"
--          ++ "set notics;"
--          ++ "splot ")
--          (cmd ++ [(pts,"notitle 'v' with points 3")])

shcam :: Mat -> [[Double]]
shcam p = c where
   (h,f) = toCameraSystem p
   c = ht (h <> diag (fromList [1,1,1,15])) (cameraOutline' f)

drawCameras :: String -> [Mat] -> [[Double]] -> IO ()
drawCameras tit ms pts = do
  let cmd = map (f.shcam) ms
      f c = (c,"notitle 'c1' with lines 1")

  gnuplotWin tit
         (  "set view 72,200; "
         ++ "set pointsize 0.1;"
         ++ "set xlabel 'x'; set ylabel 'y'; set zlabel 'z';"
         ++ "set xrange [-2:2]; set yrange [-2:2]; set zrange [-2:2];"
         ++ "set size ratio 1;"
         ++ "set ticslevel 0;"
         ++ "set notics;"
         ++ "splot ")
         (cmd ++ [(pts,"notitle 'v' with points 7")])

cameraOutline' :: Double -> [[Double]]
cameraOutline' f =  [0::Double,0,0] : drop 5 (cameraOutline f)

--------------------------------------------------------------------------------

theRightB b c = k <> (rt + (3><4) [0,0,0,b,0,0,0,0,0,0,0,0])
  where
    (k,rt) = sepCam c

theRight :: Camera -> Camera
theRight = unsafeMap (theRightB 1)

--------------------------------------------------------------------------------

newtonStep :: Double -> (Vec -> (Vec, Mat)) -> Vec -> Vec
newtonStep lambda m sol = sol'
  where
    (fun,jac) = m sol
    hes = trans jac <> jac
    grad = trans jac <> fun
    sol' = sol - (aug lambda hes) <\> grad
    aug lam mat = mat + diag (constant lam (rows mat))


mkJac cam views points = (origin, g)
  where
    origin = cameraModelOrigin (Just (ident 3)) cam
    preJaco = auxCamJac origin
    g s = ( (subtract vs) . vjoin *** fromRows . concatMap toRows) $ unzip $ map h (toRows points)
      where
        jac = preJaco s
        vs = flatten $ inhomog views
        h x = projectionDerivAtH jac x


refineNewton n cam views points = k <> result
  where
    result = projectionAt'' model $ (!!n) $ iterate (newtonStep 0 funJac) (konst 0 6)
    (model,funJac) = mkJac kcam kviews points
    (k,kcam) = sepCam cam
    kviews = views <> trans (inv k)

