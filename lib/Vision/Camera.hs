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
, poseFromFactorization
, poseFromCamera
, homogZ0
, focalFromHomogZ0
, cameraFromHomogZ0
, poseFromHomogZ0
, estimateCameraFromPlane
, kgen
, cameraOutline
, toCameraSystem
, estimateCamera
, estimateCameraRaw
) where

import GSL hiding (Matrix, Vector)
import qualified GSL as G
import Vision.Geometry
import Vision.Estimation(homogSystem, withNormalization, estimateHomography)
import Classifier.Stat
import Data.List(transpose,nub,maximumBy,genericLength,elemIndex, genericTake)
import System.Random 
import Debug.Trace(trace)

type Matrix = G.Matrix Double
type Vector = G.Vector Double

matrix = fromLists :: [[Double]] -> Matrix
vector = fromList ::  [Double] -> Vector

svd = svdR'

(!:) = (@>)
(!!:) = (@@>)

cameraAtOrigin = ident 3 <|> vector [0,0,0]

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
easyCamera fov cen@(cx,cy,cz) pun@(px,py,pz) rho  = 
    CamPar { focalDist = f
           , panAngle = beta
           , tiltAngle = alpha
           , rollAngle = rho
           , cameraCenter = cen
           } where 
    dx = px-cx
    dy = py-cy
    dz = pz-cz
    dh = sqrt (dx*dx+dy*dy)
    f = 1 / tan (fov/2)
    beta = atan2 (-dx) dy
    alpha = atan2 dh (-dz) 

-- | Obtains the 3x4 homogeneous transformation from world points to image points.
syntheticCamera :: CameraParameters -> Matrix
syntheticCamera campar = flipx <> k <> r <> m where
    CamPar {focalDist = f, 
            panAngle = p, tiltAngle = t, rollAngle = q,
            cameraCenter = (cx,cy,cz)} = campar
    m = matrix [[1,0,0, -cx],
                    [0,1,0, -cy],
                    [0,0,1, -cz]]
    r = rotPTR (p,t,q)
    k = kgen f

flipx = diag (vector [-1,1,1])

-- | Matrix of intrinsic parameters of a diag(f,f,1) camera
kgen :: Double -> Matrix
kgen f = matrix [[f,0,0],
                     [0,f,0],
                     [0,0,1]]


rotPTR (pan,tilt,roll) = matrix
   [[-cb*cg + ca*sb*sg, -cg*sb - ca*cb*sg, -sa*sg],
    [ ca*cg*sb + cb*sg, -ca*cb*cg + sb*sg, -cg*sa],
    [            sa*sb,            -cb*sa,     ca]]
   where cb = cos pan 
         sb = sin pan 
         ca = cos tilt
         sa = sin tilt
         cg = cos roll
         sg = sin roll


focal' c = res where
    n = c <> mS <> trans c <> linf
    d = c <> mA <> trans c <> linf
    x = c <> mF <> trans c <> linf
    xi = inHomog x
    ni = inHomog n
    f = sqrt $ norm (xi - ni) ^2 - norm ni ^2
    res = if f > 0 then Just f else Nothing 

-- | Tries to compute the focal dist of a camera given the homography Z0 -> image
focalFromHomogZ0 :: Matrix -> Maybe Double
focalFromHomogZ0 c = res where
    [a11,a12,a13, 
     a21,a22,a23, 
     a31,a32,a33] = toList (flatten c)
    nix = (a11*a31 + a12 *a32)/den 
    niy = (a21*a31 + a22 *a32)/den
    xix = (a12 *(-a31 + a32) + a11 *(a31 + a32))/den
    xiy = (a22 *(-a31 + a32) + a21 *(a31 + a32))/den
    den = a31^2 + a32^2
    f = sqrt $ (xix-nix)^2 +(xiy-niy)^2 - nix^2 - niy^2
    res = if f > 0 then Just f else Nothing

-- | Obtains the pose of a factorized camera.
poseFromFactorization :: (Matrix,Matrix,Vector)  -- ^ (k,r,c) as obtained by factorizeCamera
                         -> CameraParameters
poseFromFactorization (k,r,c) = cp where
    cp = CamPar {focalDist = f,
                 panAngle = -beta,
                 tiltAngle = alpha,
                 rollAngle = -rho,
                 cameraCenter = (cx, cy, cz) }
    f = (k!!:(0,0)+k!!:(1,1))/2 -- hmm
    [cx,cy,cz] = toList c
    [r1,r2,r3] = toColumns r
    h = fromColumns [r1,r2,-r<>c]
    b = trans h <> linf
    beta = atan2 (b!:0) (b!:1)
    n = k <> h <> mS <> b
    ni = inHomog n
    rho = atan2 (ni!:0) (ni!:1)
    alpha = atan2 f (norm ni)

-- | Extracts the camera parameters of a diag(f,f,1) camera
poseFromCamera :: Matrix               -- ^ 3x4 camera matrix
                  -> CameraParameters
poseFromCamera = poseFromFactorization . factorizeCamera

-- | Tries to extract the pose of the camera from the homography of the floor
poseFromHomogZ0 :: Maybe Double      -- ^ focal distance (if known)
           -> Matrix                      -- ^ 3x3 floor to image homography
           -> Maybe CameraParameters      -- ^ solution (the one above the floor)
poseFromHomogZ0 mbf = fmap poseFromCamera . cameraFromHomogZ0 mbf

--degree = pi / 180

extractColumns cs = trans . extractRows cs . trans

-- | Obtains the homography floor (Z=0) to image from a camera
homogZ0 :: Matrix -> Matrix
homogZ0 cam = extractColumns [0,1,3] cam

-- | Recovers a camera matrix from the homography floor (Z=0) to image. There are actually two solutions, above and below the ground. We return the camera over the floor
cameraFromHomogZ0 :: Maybe Double              -- ^ focal distance (if known)
           -> Matrix                           -- ^ 3x3 floor to image homography
           -> Maybe Matrix                     -- ^ 3x4 camera matrix (solution over the floor)
cameraFromHomogZ0 mbf c = res where
    mf = case mbf of
            Nothing -> focalFromHomogZ0 c     -- unknown, we try to estimate it
            jf -> jf                          -- given, use it
    res = case mf of
            Just f -> Just m      -- solution
            Nothing -> Nothing    -- cannot be estimated
    Just f = mf
    s = kgen (1/f) <> c
    [s1,s2,s3] = toColumns s
    sc = norm s1
    t = s3 <> recip sc
    r1 = unitary s1
    r3 = unitary (cross s1 s2)
    r2 = cross r3 r1
    rot1 = fromColumns [r1,r2,r3]
    cen1 = - (trans rot1 <> t)
    m1 = kgen f <> (fromColumns [r1,r2,r3] <|> t)
    m2 = kgen f <> (fromColumns [-r1,-r2,r3] <|> -t)
    m = if cen1!:2 > 0 then m1 else m2





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

toCameraSystem cam = (inv m, f) where
    (k,r,c) = factorizeCamera cam
    m = (r <|> -r <> c) <-> vector [0,0,0,1]
    (f:_):_ = toLists k

doublePerp (a,b) (c,d) = (e,f) where
    a' = vector a
    b' = vector b
    c' = vector c
    d' = vector d
    v = cross (b'-a') (d'-c')
    coef = fromColumns [b'-a', v, c'-d']
    term = c'-a'
    [lam,mu,ep] = toList (inv coef <> term)
    e = toList $ a' + lam <> (b'-a')
    f = toList $ a' + lam <> (b'-a') + mu <> v

------------------------------------------------------            

-- | The RQ decomposition, written in terms of the QR. 
rq :: Matrix -> (Matrix,Matrix) 
rq m = (r,q) where
    (q',r') = qr $ trans $ rev1 m
    r = rev2 (trans r')
    q = rev2 (trans q')
    rev1 = flipud . fliprl
    rev2 = fliprl . flipud


-- | Given a camera matrix m it returns (K, R, C)
--   such as m \=\~\= k \<\> r \<\> (ident 3 \<\|\> -c)
factorizeCamera :: Matrix -> (Matrix,Matrix,Vector)
factorizeCamera m = (normat3 k, r <> signum (det r),c) where
    m' = takeColumns 3 m
    (k',r') = rq m'
    s = diag(signum (takeDiag k'))
    (_,_,v) = svd m
    (v',_) = qr v
    k = k'<>s
    r = s<>r'
    c = inHomog $ flatten $ dropColumns 3 v'

estimateCameraRaw image world = h where
    eqs = concat (zipWith eq image world)
    h = reshape 4 $ homogSystem eqs
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

estimateCamera = withNormalization inv estimateCameraRaw 

----------------------------------------------------------

-- | Estimation of a camera matrix from image - world correspondences, for world points in the plane z=0. We start from the closed-form solution given by 'estimateHomography' and 'cameraFromHomogZ0', and then optimize the camera parameters by minimization (using the Nelder-Mead simplex algorithm) of reprojection error.
estimateCameraFromPlane
                :: Maybe Double  -- ^ focal dist, if known
                -> [[Double]]    -- ^ image points as [x,y]
                -> [[Double]]    -- ^ world points in plane z=0, as [x,y]
                -> Maybe Matrix  -- ^ 3x4 camera matrix
estimateCameraFromPlane mbf image world = c where
    h = estimateHomography image world
    c = case cameraFromHomogZ0 mbf h of
        Nothing -> Nothing
        Just p  -> Just $ fst $ refine p mimage mworld
                     where refine = case mbf of Nothing -> refineCamera1
                                                _       -> refineCamera2
    mimage = fromLists image
    mworld = fromLists (map pl0 world)
    pl0 [x,y] = [x,y,0]


refineCamera1 cam mview mworld = (betterCam,path) where
    initsol = par2list $ poseFromCamera cam
    (betterpar, path) = minimize (cost mview mworld) initsol
    betterCam = syntheticCamera $ list2par betterpar
    cost view world lpar = pnorm 2 $ flatten (view - htm c world)
        where c = syntheticCamera $ list2par lpar
    minimize f xi = minimizeNMSimplex f xi [0.01,5*degree,5*degree,5*degree,0.1,0.1,0.1] 1e-3 500

refineCamera2 cam mview mworld = (betterCam,path) where
    f:initsol = par2list $ poseFromCamera cam
    (betterpar, path) = minimize (cost mview mworld) initsol
    betterCam = syntheticCamera $ list2par (f:betterpar)
    cost view world lpar = pnorm 2 $ flatten (view - htm c world)
        where c = syntheticCamera $ list2par (f:lpar)
    minimize f xi = minimizeNMSimplex f xi [5*degree,5*degree,5*degree,0.1,0.1,0.1] 1e-3 500

list2par [f,p,t,r,cx,cy,cz] = CamPar f p t r (cx,cy,cz)
par2list (CamPar f p t r (cx,cy,cz)) = [f,p,t,r,cx,cy,cz]
