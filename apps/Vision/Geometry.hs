-----------------------------------------------------------------------------
{- |
Module      :  Vision.Geometry
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Visual geometry.

-}
-----------------------------------------------------------------------------

module Vision.Geometry
(
  -- * Camera synthesis and analysis
  CameraParameters(..)
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
  -- * Useful transformations
, kgen
, rot1
, rot2
, rot3
, desp
, scaling
, mS
, mA
, linf
, cross
, unitary
, homog
, inHomog
, degree
, normatdet
, normat
, normat3
, ht
, htc
 -- Estimation of transformations
, estimateHomographyRansac
, estimateHomography
, estimateHomographyRaw
, estimateCamera
, estimateCameraRaw
, toCameraSystem
, cameraOutline 
-- Stereo Geometry
, estimateFundamental
, epipolarQuality
, qualityOfEssential
, estimateEssential
, estimateEssential'
, camerasFromEssential
, bougnoux
, sturm
, selectCamera
, triangulate
, stereoRectifiers
, epipoles
) where

import GSL hiding (Matrix, Vector)
import qualified GSL as G
import Vision.Stat
import Data.List(transpose,nub,maximumBy,genericLength,elemIndex, genericTake)
import System.Random 
import Debug.Trace(trace)

type Matrix = G.Matrix Double
type Vector = G.Vector Double

matrix = fromLists :: [[Double]] -> Matrix
vector = fromList ::  [Double] -> Vector


(!:) = (@>)
(!!:) = (@@>)


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

-- | 3x3 rotation around the X-axis
rot1 :: Double -> Matrix
rot1 a = matrix [[1, 0,0],
                     [0, c,s],
                     [0,-s,c]] 
    where c = cos a 
          s = sin a

-- | 3x3 rotation around the Y-axis
rot2 :: Double -> Matrix
rot2 a = matrix [[ c,0,s],
                     [ 0,1,0],
                     [-s,0,c]] 
    where c = cos a 
          s = sin a

-- | 3x3 rotation around the Z-axis
rot3 :: Double -> Matrix
rot3 a = matrix [[ c,s,0],
                     [-s,c,0],
                     [ 0,0,1]] 
    where c = cos a 
          s = sin a

-- | Homogeneous 3x3 matrix of a 2D displacement
desp :: (Double,Double) -- ^ (dx,dy)
      -> Matrix
desp (x,y) = matrix [[1,0,x],
                       [0,1,y],
                       [0,0,1]]

-- | Homogeneous 3x3 matrix of a isotropic 2D scaling
scaling :: Double -> Matrix
scaling s = matrix [[s,0,0],
                        [0,s,0],
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

-- | Antisymmetric matrix 0 0 1
mA :: Matrix
mA = matrix [[ 0,1,0],
                 [-1,0,0],
                 [ 0,0,0]] 

-- | diag(1,1,0)
mS :: Matrix
mS = matrix [[1,0,0],
                 [0,1,0],
                 [0,0,0]] 

mF = matrix [[ 1,1,0],
                 [-1,1,0],
                 [ 0,0,0]] 

-- | the line of the infinity.
linf :: Vector
linf = vector [0,0,1]

inHomog v = subVector 0 l v <> recip (v!:l) where l = size v - 1
homog v = join [v,1]

-- | Obtains a vector in the same direction with 2-norm=1
unitary:: Vector -> Vector
unitary v = v <> recip (norm v)

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

degree = pi / 180

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




asMat v = matrix [[ 0,-c, b],
                  [ c, 0,-a],
                  [-b, a, 0]]
    where a = v!:0
          b = v!:1
          c = v!:2     

cross a b = asMat a <> b    

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

-- overconstrained nullspace (mse solution of homogeneous linear system)
-- we assume that it is 1d 
homogSystem :: [[Double]] -> Vector
homogSystem coeffs = sol where
    r = length coeffs
    c = length (head coeffs)
    mat | r >= c   = matrix coeffs
        | r == c-1 = matrix (head coeffs : coeffs)
        | otherwise = error "homogSystem with rows<cols-1"
    (_,_,v) = svd mat
    sol = flatten $ dropColumns (c-1) v

estimateHomographyRaw dest orig = h where
    eqs = concat (zipWith eq dest orig)
    h = reshape 3 $ homogSystem eqs
    eq [bx,by] [ax,ay] = 
        [[  0,  0,  0,t14,t15,t16,t17,t18,t19],
         [t21,t22,t23,  0,  0,  0,t27,t28,t29],
         [t31,t32,t33,t34,t35,t36,  0,  0,  0]] where
            t14=(-ax)
            t15=(-ay)
            t16=(-1)
            t17=by*ax 
            t18=by*ay 
            t19=by
            t21=ax 
            t22=ay 
            t23=1
            t27=(-bx*ax) 
            t28=(-bx*ay) 
            t29=(-bx)
            t31=(-by*ax) 
            t32=(-by*ay) 
            t33=(-by)
            t34=bx*ax 
            t35=bx*ay
            t36=bx     

withNormalization lt estimateRelation dest orig = lt wd <> h <> wo where
    std = stat (matrix dest)
    sto = stat (matrix orig)
    nd = toLists (normalizedData std)
    no = toLists (normalizedData sto)
    h = estimateRelation nd no
    wd = whiteningTransformation std
    wo = whiteningTransformation sto 

estimateHomography = withNormalization inv estimateHomographyRaw   

normat3 m = m <> recip m!!:(rows m -1, cols m -1)

normatdet m = m <> recip k where
    s = subMatrix (0,0) (n,n) m
    n = min (rows m) (cols m)
    d = det s
    k = signum d * abs d **(1/ fromIntegral n)

normat m = m <> recip (norm (flatten m))

homogMat m = m <|> constant 1 (rows m)

inHomogMat m = ma / (mb `outer` constant 1 (cols ma))
    where ma = takeColumns (cols m -1) m
          mb = flatten $ dropColumns (cols m -1) m


htc h = toLists. inHomogMat . (<> trans h) . homogMat . fromLists

ht :: Matrix -> [[Double]] -> [[Double]]
ht = htc

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

--------------------- Basic Stereo -----------------------

estimateFundamentalRaw l r = f where
    f = reshape 3 $ homogSystem eqs
    eqs = zipWith eq l r
    eq [xl, yl] [xr, yr] = [xl*xr, xl*yr, xl, xr*yl, yl*yr, yl, xr, yr, 1.0]

correctFundamental f = f' where
    (u,s,v) = svd f
    s1:s2:_ = toList s
    f' = u <> diag (fromList [s1,s2,0.0]) <> trans v

estimateFundamental = withNormalization trans f where
    f l r = correctFundamental (estimateFundamentalRaw l r)

distPointLine [x,y,w] [a,b,c] = sqrt $ (a*x + b*y + c*w)^2 / (a^2+b^2) / w^2

epipolarQuality f l r = zipWith fun l r where
    fun [a1,a2] [b1,b2] = distPointLine [a1,a2,1.0] epb where
        epb = toList (f <> vector [b1,b2,1.0])

qualityOfEssential e = (s1-s2)/(s1+s2) where
    s1:s2:_ = toList s
    (_,s,_) = svd e

estimateEssential f0 fund = (esen,f,err) where
    minimize fun xi = minimizeNMSimplex fun xi (replicate (length xi) 1) 1e-2 100
    cost [x] = qualityOfEssential (kgen x <> fund <> kgen x)
    ([f],_) = minimize cost [f0]
    err = cost [f]
    esen' = kgen f <> fund <> kgen f
    (u,s,v) = svd esen'
    esen = u <> diag (vector [1,1,0]) <> trans v

estimateEssential' (f0,f0') fund = (esen,(f,f'),err) where
    minimize fun xi = minimizeNMSimplex fun xi (replicate (length xi) 1) 1e-2 100
    cost [x,x'] = qualityOfEssential (kgen x' <> fund <> kgen x)
    ([f,f'],_) = minimize cost [f0,f0']
    err = cost [f,f']
    esen' = kgen f' <> fund <> kgen f
    (u,s,v) = svd esen'
    esen = u <> diag (vector [1,1,0]) <> trans v

bougnoux fun = sqrt (- a / b) where
    a = (p' <> asMat e' <> i' <> fun <> p) * (p <> trans fun <> p')
    b = (p' <> asMat e' <> i' <> fun <> i' <> trans fun <> p')
    (_,e') = epipoles fun
    i' = diag $ vector [1,1,0]
    p = vector [0,0,1]
    p' = vector [0,0,1]

sturm fun = fs where    
    (u,s,v) = svd fun
    [a,b,_] = toList s
    [[u11, u12, u13],
     [u21, u22, u23],
     [  _,   _,   _]] = toLists (trans u)
    [[v11, v12, v13],
     [v21, v22, v23],
     [  _,   _,   _]] = toLists (trans v) 
    k = a*u13*v13+b*u23*v23
    p0 = u23*v13*k
    q0 = u13*v23*k
    p1 = a*u13*u23*(1-v13^2) + b*v13*v23*(1-u23^2)
    q1 = a*v13*v23*(1-u13^2) + b*u13*u23*(1-v23^2)
    r0 = a^2*u13^2*v13^2 - b^2*u23^2*v23^2
    r1 = a^2*(u13^2+v13^2-2*u13^2*v13^2) - b^2*(u23^2+v23^2-2*u23^2*v23^2)
    r2 = a^2*(1-u13^2)*(1-v13^2) - b^2*(1-u23^2)*(1-v23^2)
    f1 = sqrt (-p0/p1)
    f2 = sqrt (-q0/q1)
    d = sqrt (r1^2 - 4*r0*r2)
    f3 = sqrt $ (-r1 + d)/2/r2
    f4 = sqrt $ (-r1 - d)/2/r2
    fs = filter (>0) [f1,f2,f3,f4]

camerasFromEssential e = [m1,m2,m3,m4] where
    (u,_,v) = svd e
    [_,_,u3] = toColumns u
    w = matrix [[ 0,1,0],
                    [-1,0,0],
                    [ 0,0,1]]
    m1 = u <>       w <> trans v <|>  u3
    m2 = u <>       w <> trans v <|> -u3
    m3 = u <> trans w <> trans v <|>  u3
    m4 = u <> trans w <> trans v <|> -u3

triangulate1 ms ps = x3d where
    eq [[m11, m12, m13, m14],
        [m21, m22, m23, m24],
        [m31, m32, m33, m34]] [x,y] = [[ m21-y*m31,   m22-y*m32,    m23-y*m33,   m24-y*m34],
                                       [-m11+x*m31,  -m12+x*m32,   -m13+x*m33,  -m14+x*m34],
                                       [y*m11-x*m21, y*m12-x*m22, y*m13-x*m23, y*m14-x*m24]]
    eqs = concat $ zipWith eq (map toLists ms) ps
    a = matrix eqs
    (_,_,v) = svd a
    x3d = toList $ inHomog $ flatten $ dropColumns 3 v

triangulate mps = xs where
    ms = map fst mps
    ps = transpose (map snd mps)
    xs = map (triangulate1 ms) ps

cameraAtOrigin = ident 3 <|> vector [0,0,0]

cameraDirection m = unitary (det a <> m3) where
    a = takeColumns 3 m
    [_,_,m3] = toRows a

--depthOfPoint :: [Double] -> Matrix -> Double
depthOfPoint p m = (signum (det a) / norm m3) <> w3 where
    a = takeColumns 3 m
    [_,_,m3] = toRows a
    w = m <> homog (vector p)
    [_,_,w3] = toList w

depthsOfInducedPoint p p' m m' = (d,d') where
    d  = depthOfPoint x m
    d' = depthOfPoint x m'
    x = triangulate1 [m,m'] [p,p']

selectCamera p p' m ms = m' where
    [m'] = filter f ms 
    f m' = a > 0 && b > 0 where
        (a,b) = depthsOfInducedPoint p p' m m'

epipoles f = (nullspace f, nullspace (trans f)) where
    nullspace f = flatten $ dropColumns 2 v where (_,_,v) = svd f

canonicalCameras f = (cameraAtOrigin, m2) where
    (_,e') = epipoles f
    m2 = asMat e' <> f  <|> e'

fundamentalFromCameras p p' = asMat e' <> p' <> pinv p where
    e' = p' <> c
    c = flatten $ dropColumns 3 v 
    (_,_,v) = svd (p <-> vector [0,0,0,0])

stereoRectifiers fund pts pts' = (h,h') where    -- HZ p.307
    (e,e') = epipoles fund
    [x,y,w] = toList (unitary e')
    x' = x/w
    y' = y/w
    roll = if abs w < 1E-6
        then atan2 y x
        else atan2 y' x'
    r = rot3 (roll)
    q = sqrt (x'^2 + y'^2)
    g = matrix [[1,0,0],  -- HZ p.305, better than a conjugate rotation
                [0,1,0],
                [a,0,1]] where a = -1.0/q
    h' = g <> r

    h = ha <> h0
    h0 = h' <> m
    (_,m') = canonicalCameras fund
    m = takeColumns 3 m' + outer e' (vector [1,1,1]) -- hmmm
    ha = matrix [[a,b,c],
                     [0,1,0],
                     [0,0,1]]
    t' = ht h' pts'
    t =  ht h0 pts
    eq [x,y] [x',_] = [x,y,1,x']
    eqs = matrix $ zipWith eq t t'
    coef = takeColumns 3 eqs
    term = flatten $ dropColumns 3 eqs
    [a,b,c] = toList $ pinv coef <> term

------------------------------ RANSAC -------------------------------

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)
-- take (length l `quot`n) $ unfoldr (\a -> Just (splitAt n a)) l   

compareBy f = (\a b-> compare (f a) (f b))

ransac' :: ([a]->t) -> (t -> a -> Bool) -> Int -> Int -> [a] -> (t,[a])
ransac' estimator isInlier n t dat = (result, goodData) where
    result = estimator goodData
    goodData = inliers bestModel
    bestModel = maximumBy (compareBy (length.inliers)) models
    models = take t (map estimator (samples n dat))
    inliers model = filter (isInlier model) dat

-- | @samples n list@ creates an infinite list of psuedorandom (using mkStdGen 0) subsets of n different elements taken from list
samples :: Int -> [a] -> [[a]]
samples n dat = map (map (dat!!)) goodsubsets where
    goodsubsets = filter ((==n).length) $ map nub $ partit n randomIndices
    randomIndices = randomRs (0, length dat -1) (mkStdGen 0)

ransacSize s p eps = 1 + (floor $ log (1-p) / log (1-(1-eps)^s))    ::Integer

position fun l = k where Just k = elemIndex (fun l) l


-- | adaptive ransac
ransac :: ([a]->t) -> (t -> a -> Bool) -> Int -> [a] -> (t,[a])
ransac estimator isInlier n dat = {-trace (show aux)-} (bestModel,inliers) where 
    models = map estimator (samples n dat)
    inls = map inliers models where inliers model = filter (isInlier model) dat 
    eps = map prop inls where prop l = 1 - genericLength l / genericLength dat
    ns = scanl1 min $ map (ransacSize n 0.99) eps 
    k = fst $ head $ dropWhile (\(k,n) -> k<n) (zip [1 ..] ns)
    p = position maximum (map length (genericTake k inls))
    bestModel = models!!p
    inliers = inls!!p
    aux = map length $ genericTake k inls

--------------------------    

isInlierTrans t h (dst,src) = norm (vd - vde) < t 
    where vd  = vector dst
          vde = inHomog $ h <> homog (vector src)

estimateHomographyRansac dist dst orig = h where 
    h = estimateHomography a b where (a,b) = unzip inliers
    (_,inliers) = ransac estimator (isInlierTrans dist) 4 (zip dst orig)
    estimator l = estimateHomographyRaw a b where (a,b) = unzip l

isInlierFund t f (x',x) = head (epipolarQuality f [x'] [x]) < t   

estimateFundamentalRansac dist pts' pts = (f,inliers) where 
    f = estimateFundamental a b where (a,b) = unzip inliers
    (_,inliers) = ransac estimator (isInlierFund dist) 8 (zip pts' pts)
    estimator l = estimateFundamentalRaw a b where (a,b) = unzip l
