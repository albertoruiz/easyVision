module Vision where

import GSL
import Stat
 
data CameraParameters
    = CamPar { focalDist
             , panAngle
             , tiltAngle
             , rollAngle
             , centerX
             , centerY
             , centerZ :: Double
             } deriving Show
 
-- computes the camera parameters from the camera center and the point imaged in the image center  
easyCamera :: Double -> (Double,Double,Double) -> (Double,Double,Double) -> Double -> CameraParameters
easyCamera fov cen@(cx,cy,cz) pun@(px,py,pz) rho  = 
    CamPar { focalDist = f
           , panAngle = beta
           , tiltAngle = alpha
           , rollAngle = rho
           , centerX = cx
           , centerY = cy
           , centerZ = cz
}
    where dx = px-cx
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
            centerX = cx, centerY = cy, centerZ = cz} = campar
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

det3 m 
    | rows m /=3 || cols m /= 3 = det m
    | otherwise = num
    where [m11,m12,m13, m21,m22,m23, m31,m32,m33] = toList (flatten m)
          num = -m13*m22*m31
                +m12*m23*m31
                +m13*m21*m32
                -m11*m23*m32 
                -m12*m21*m33 
                +m11*m22*m33    

inv3 m 
    | rows m /=3 || cols m /= 3 = m <\> ident (rows m)
    | otherwise = fromList [[a11,a12,a13], 
                            [a21,a22,a23], 
                            [a31,a32,a33]]
    where   [m11,m12,m13, m21,m22,m23, m31,m32,m33] = toList (flatten m)
            d = recip (det3 m)
            a11 = (-m23*m32 + m22*m33)*d
            a12 = ( m13*m32 - m12*m33)*d
            a13 = (-m13*m22 + m12*m23)*d
            a21 = ( m23*m31 - m21*m33)*d
            a22 = (-m13*m31 + m11*m33)*d
            a23 = ( m13*m21 - m11*m23)*d
            a31 = (-m22*m31 + m21*m32)*d
            a32 = ( m12*m31 - m11*m32)*d
            a33 = (-m12*m21 + m11*m22)*d

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
                 centerX = cx, centerY = cy, centerZ = -cz} 
    
    mf = case mbf of
            Just givenf -> givenf  -- given, use it
            Nothing -> focal c     -- unknown, we try to estimate it
    res = case mf of
            Just f -> Just cp           -- solution
            Nothing -> Nothing     -- cannot be estimated
    Just f = mf        
    cnor = c <> signum (det3 c)
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
    
-- with normalization    
estimateHomography' dest orig = inv3 wd <> h <> wo where
    std = stat dest
    sto = stat orig
    nd = toList (normalizedData std)
    no = toList (normalizedData sto)
    h = estimateHomographyRaw nd no
    wd = whiteningTransformation std
    wo = whiteningTransformation sto   
    
normat3 m = m <> recip m!!:(2,2)

normatdet m = m <> recip (det3 m)


homogMat m = fromBlocks [[m, constant 1 (rows m, 1::Int)]]

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
-- | m' =~= k <> r <> fromBlocks [[ident 3,reshape 1 (-c)]]
factorizeCamera :: Matrix -> (Matrix,Matrix,Vector)
factorizeCamera m = (normat3 k, r <> signum (det3 r),c) where
    m' = takeColumns 3 m
    (k',r') = rq m'
    s = diag(signum (diag k'))
    (_,_,v) = svd m
    (v',_) = qr v
    k = k'<>s
    r = s<>r'
    c = inHomog $ flatten $ dropColumns 3 v'