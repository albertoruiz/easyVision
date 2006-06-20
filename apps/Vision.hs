module Vision where

import GSL
import Stat
import Data.List(transpose)
 
data CameraParameters
    = CamPar { focalDist                      :: Double
             , panAngle, tiltAngle, rollAngle :: Double
             , cameraCenter                   :: (Double,Double,Double)
             } deriving Show
 
-- computes the camera parameters given the projection center, 
-- the point imaged in the image center and roll angle
easyCamera :: Double -> (Double,Double,Double) -> (Double,Double,Double) -> Double -> CameraParameters
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
 
 
 
kgen f = realMatrix [[f,0,0],
                     [0,f,0],
                     [0,0,1]]

rot1 a = realMatrix [[1, 0,0],
                     [0, c,s],
                     [0,-s,c]] 
    where c = cos a 
          s = sin a
    
rot2 a = realMatrix [[ c,0,s],
                     [ 0,1,0],
                     [-s,0,c]] 
    where c = cos a 
          s = sin a
    
rot3 a = realMatrix [[ c,s,0],
                     [-s,c,0],
                     [ 0,0,1]] 
    where c = cos a 
          s = sin a
    
rotPTR (pan,tilt,roll) = realMatrix 
   [[-cb*cg + ca*sb*sg, -cg*sb - ca*cb*sg, -sa*sg],
    [ ca*cg*sb + cb*sg, -ca*cb*cg + sb*sg, -cg*sa],
    [            sa*sb,            -cb*sa,     ca]]
   where cb = cos pan 
         sb = sin pan 
         ca = cos tilt
         sa = sin tilt
         cg = cos roll
         sg = sin roll
    
syntheticCamera campar = k <> r <> m where
    CamPar {focalDist = f, 
            panAngle = p, tiltAngle = t, rollAngle = q,
            cameraCenter = (cx,cy,cz)} = campar
    m = realMatrix [[1,0,0, -cx],
                    [0,1,0, -cy],
                    [0,0,1, -cz]]
    r = rotPTR (p,t,q)
    k = kgen f
          
mA = realMatrix [[ 0,1,0],
                 [-1,0,0],
                 [ 0,0,0]] 

mS = realMatrix [[1,0,0],
                 [0,1,0],
                 [0,0,0]] 

mF = realMatrix [[ 1,1,0],
                 [-1,1,0],
                 [ 0,0,0]] 

linf = realVector [0,0,1]

inHomog v = subVector 0 l v <> recip (v!:l) where l = size v - 1
homog v = join [v,1]

unitary v = v <> recip (norm v)


focal' c = res where
    n = c <> mS <> trans c <> linf
    d = c <> mA <> trans c <> linf
    x = c <> mF <> trans c <> linf
    xi = inHomog x
    ni = inHomog n
    f = sqrt $ norm (xi - ni) ^2 - norm ni ^2
    res = if f > 0 then Just f else Nothing 
 
focal c = res where
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
 
 
 
poseGen mbf c = res where
    cp = CamPar {focalDist = f, 
                 panAngle = -beta, tiltAngle = alpha, rollAngle = rho,
                 cameraCenter = (cx, cy, -cz) } 
    
    mf = case mbf of
            Just givenf -> givenf  -- given, use it
            Nothing -> focal c     -- unknown, we try to estimate it
    res = case mf of
            Just f -> Just cp           -- solution
            Nothing -> Nothing     -- cannot be estimated
    Just f = mf        
    cnor = c <> signum (det c)
    s = kgen (1/f) <> cnor
    [s1,s2,s3] = toColumns s
    r1 = unitary s1
    r3 = unitary (cross s1 s2)
    r2 = cross r3 r1
    rot = fromColumns [r1,r2,r3]
    cen = - (trans rot <> (s3 <> recip (norm s1)))
    [cx,cy,cz] = toList cen
    b = trans cnor <> linf
    beta = atan2 (b!:0) (b!:1)
    n = cnor <> mS <> b
    ni = inHomog n
    rho = atan2 (ni!:0) (ni!:1)
    alpha = atan2 f (norm ni)
        
    

degree = pi / 180    
    
extractColumns cs = trans . extractRows cs . trans    
    
homogZ0 cam = extractColumns [0,1,3] cam    
    
asMat v = fromList [[ 0,-c, b],
                    [ c, 0,-a],
                    [-b, a, 0]]
    where a = v!:0
          b = v!:1
          c = v!:2     
    
cross a b = asMat a <> b    
            
-- check equal size    
estimateHomographyRaw dest orig = h where
    eqs = concat (zipWith eq dest orig)
    a = realMatrix eqs
    (_,_,v) = svd a
    h = reshape 3 $ flatten $ dropColumns 8 v
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
    std = stat (realMatrix dest)
    sto = stat (realMatrix orig)
    nd = toList (normalizedData std)
    no = toList (normalizedData sto)
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


ht h = toList. inHomogMat . (<> trans h) . homogMat . fromList
    
-- | The RQ decomposition, written in terms of the QR. 
rq :: Matrix -> (Matrix,Matrix) 
rq m = (r,q) where
    (q',r') = qr $ trans $ rev1 m
    r = rev2 (trans r')
    q = rev2 (trans q')
    rev1 = flipud . fliprl
    rev2 = fliprl . flipud


-- | Given a camera matrix it returns (K, R, C)
-- | m' =~= k <> r <> (ident 3 <|> -c)
factorizeCamera :: Matrix -> (Matrix,Matrix,Vector)
factorizeCamera m = (normat3 k, r <> signum (det r),c) where
    m' = takeColumns 3 m
    (k',r') = rq m'
    s = diag(signum (diag k'))
    (_,_,v) = svd m
    (v',_) = qr v
    k = k'<>s
    r = s<>r'
    c = inHomog $ flatten $ dropColumns 3 v'
    
    
    
estimateCameraRaw image world = h where
    eqs = concat (zipWith eq image world)
    a = realMatrix eqs
    (_,_,v) = svd a
    h = reshape 4 $ flatten $ dropColumns 11 v
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
    f = reshape 3 $ flatten $ dropColumns 8 v
    (_,_,v) = svd eqs
    eqs = realMatrix (zipWith eq l r)
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
        epb = toList (f <> realVector [b1,b2,1.0])
        
qualityOfEssential e = (s1-s2)/(s1+s2) where
    s1:s2:_ = toList s
    (_,s,_) = svd e         
        
qualityOfInducedEssential fund f = qualityOfEssential (kgen f <> fund <> kgen f)
            
estimateEssential f0 fund = (esen,f,err) where
    minimize fun xi = minimizeNMSimplex fun xi (replicate (length xi) 1) 1e-2 100
    ([f],_) = minimize (\[x]-> qualityOfInducedEssential fund x) [f0]
    err = qualityOfInducedEssential fund f
    esen' = kgen f <> fund <> kgen f
    (u,s,v) = svd esen'
    esen = u <> diag (realVector [1,1,0]) <> trans v
    
bougnoux fun = sqrt $ - a / b where
    a = (p' <> asMat e' <> i' <> fun <> p) * (p <> trans fun <> p')
    b = (p' <> asMat e' <> i' <> fun <> i' <> trans fun <> p')
    (_,e') = epipoles fun
    i' = diag $ realVector [1,1,0]
    p = realVector [0,0,1]
    p' = realVector [0,0,1]
    
camerasFromEssential e = [m1,m2,m3,m4] where
    (u,_,v) = svd e
    [_,_,u3] = toColumns u
    w = realMatrix [[ 0,1,0],
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
    eqs = concat $ zipWith eq (map toList ms) ps
    a = realMatrix eqs
    (_,_,v) = svd a
    x3d = toList $ inHomog $ flatten $ dropColumns 3 v
    
triangulate mps = xs where
    ms = map fst mps
    ps = transpose (map snd mps)
    xs = map (triangulate1 ms) ps
    
cameraAtOrigin = ident 3 <|> realVector [0,0,0]

cameraDirection m = unitary (det a <> m3) where
    a = takeColumns 3 m
    [_,_,m3] = toRows a
    
depthOfPoint p m = (signum (det a) / norm m3) <> w3 where
    a = takeColumns 3 m
    [_,_,m3] = toRows a
    w = m <> homog (realVector p)
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
    (_,_,v) = svd (p <-> realVector [0,0,0,0]) 
    
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
    g = realMatrix [[1,0,0],  -- HZ p.305, better than a conjugate rotation
                    [0,1,0],
                    [a,0,1]] where a = -1.0/q
    h' = g <> r
    
    h = ha <> h0
    h0 = h' <> m
    (_,m') = canonicalCameras fund
    m = takeColumns 3 m' + outer e' (realVector [1,1,1]) -- hmmm
    ha = realMatrix [[a,b,c],
                     [0,1,0],
                     [0,0,1]]
    t' = ht h' pts'
    t =  ht h0 pts
    eq [x,y] [x',_] = [x,y,1,x']
    eqs = realMatrix $ zipWith eq t t'
    coef = takeColumns 3 eqs
    term = flatten $ dropColumns 3 eqs
    [a,b,c] = toList $ pinv coef <> term
