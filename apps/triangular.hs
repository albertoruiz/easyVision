module Main where

import Vision
import GSL 

diagl = diag . realVector

flipx' = diagl [1,-1,1]

myCam (cx,cy,cz) (dx,dy,dz) = flipx' <> (syntheticCamera $ easyCamera (45*degree) (cx,cy,cz) (cx+dx,cy+dy,cz+dz) 0)

angles (cx,cy,cz) (dx,dy,dz) = (beta1/degree,beta2/degree,alpha/degree) where
    beta2 = atan2 dx dz - beta1 -pi
    alpha = atan2 dy (sqrt (dx^2+dz^2))
    beta1 = atan2 cx cz

m1 = myCam (0,0,0) (0,0,1)

m2 = myCam (-2.5,0,1) (1,0.5,2)

m3 = myCam (-4,0,2) (2,0.5,1)

m4 = myCam (1,0,1) (-1,-1,0.1)

funda = fundamentalFromCameras m1 m4
--angs = angles (-2.5,0,1) (1,0.5,2)
angs = angles (1,0,1) (-1,-1,0.1)

angledeg p r =  (/degree) $ acos $ (p<>r) / norm p / norm r

perpPointLine p l = (p `cross` (mS <> l)) 

triangular fund f = --map (/degree) [beta1, beta2, beta3, alpha2, r] where
                    r/degree where
    p = realVector [0,0,1]
    p' = p
    (e,e') = epipoles fund
    b1 = norm (inHomog e)
    b2 = norm (inHomog e')
    a1 = norm (inHomog q1)
    a2 = norm (inHomog q2)
    l = p' <> fund
    l' = fund <> p
    q2 = l' `cross` perpPointLine p' l'
    q1 = l `cross` perpPointLine p (p `cross` e)
    r = gamma1 + phi + gamma2
     
    alpha1 = atan2 a1 f
    beta1 = atan2 b1 f
    alpha2 = atan2 a2 f
    gamma2 = atan2 b2 f 
    beta2 = acos $ cos gamma2 / cos alpha2
    beta3 = pi - beta1 - beta2 
     
    gamma1 = acos $ cos alpha1 * cos beta1
    phi = acos $ sin alpha1 * sin alpha2 + cos alpha1 * cos alpha2 * cos beta3
       

main = do
    let fs = linspace 500 (0.5,5)
    mplot [fs, vmap (triangular funda) fs]
    mplot [fs, vmap (\x -> qualityOfEssential (kgen x <> funda <> kgen x)) fs]

