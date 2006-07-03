module Typical where

import Ipp
     
---------------------------------------     
     
imgAsR1 roifun im = do 
    r <- imgAs im
    return r {vroi = roifun (vroi im)} 

cr1 f im r = f // src im (vroi r) // dst r (vroi r)

imgAsR2 roifun im1 im2 = do 
    r <- imgAs im1
    return r {vroi = roifun (vroi im1) (vroi im2)}

cr2 f im1 im2 r = f // src im1 (vroi r) // src im2 (vroi r)// dst r (vroi r)

----------------------------------------
     
set32f v im roi = ippiSet_32f_C1R v // dst im roi // checkIPP "set32f" [im]

scale8u32f vmin vmax im = do
    r' <- img 4 1 (height im) (width im)
    let r = r' {vroi = vroi im}
    (cr1 ippiScale_8u32f_C1R im r) vmin vmax // checkIPP "scale8u32f" [im]
    return r
       
copyROI32f r im = ippiCopy_32f_C1R // src im (vroi im) // dst r (vroi im) // checkIPP "copyROI32f" [im]
       
simplefun1 ippfun roifun msg = g where
    g im = do
        r <- imgAsR1 roifun im
        cr1 ippfun im r // checkIPP msg [im]
        return r       
       
copy32f = simplefun1 ippiCopy_32f_C1R id "copy32f"
abs32f  = simplefun1 ippiAbs_32f_C1R  id "abs32f"    
sqrt32f = simplefun1 ippiSqrt_32f_C1R id "sqrt32f" 
sobelVert = simplefun1 ippiFilterSobelVert_32f_C1R (shrink (1,1)) "sobelVert"
sobelHoriz = simplefun1 ippiFilterSobelHoriz_32f_C1R (shrink (1,1)) "sobelHoriz"
 
gauss mask = simplefun1 f (shrink (s,s)) "gauss" where
    s = case mask of
                33 -> 1
                55 -> 2
                _  -> error "illegal mask"
    f ps ss pd sd r = ippiFilterGauss_32f_C1R ps ss pd sd r mask

thresholdVal32f t v code = simplefun1 f id "thresholdVal32f" where
    f ps ss pd sd r = ippiThreshold_Val_32f_C1R ps ss pd sd r t v code

filterMax32f sz = simplefun1 f (shrink (d,d)) "filterMax32f" where
    d = (sz-1) `quot` 2
    f ps ss pd sd r = ippiFilterMax_32f_C1R ps ss pd sd r (ippRect sz sz) (ippRect d d)
 
---------------------------------------------

simplefun2 ippfun roifun msg = g where
    g im1 im2 = do
        r <- imgAsR2 roifun im1 im2
        cr2 ippfun im1 im2 r // checkIPP msg [im1,im2]
        return r

infixl 7  |*|
infixl 6  |+|, |-|
(|*|) = simplefun2 ippiMul_32f_C1R intersection "mul32f"
(|+|) = simplefun2 ippiAdd_32f_C1R intersection "add32f"
(|-|) = flip $ simplefun2 ippiSub_32f_C1R intersection "sub32f" -- more natural argument order


compare32f code im1 im2 = do
    r <- img 1 1 (height im1) (width im1)
    let roi = intersection (vroi im1) (vroi im2)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) code // checkIPP "compare32f" [im1,im2]
    return r {vroi = roi}

copyMask32f im mask = do
    r <- imgAs im
    let roi = intersection (vroi im) (vroi mask)
    set32f 0.0 r (fullroi r)
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [im,mask]
    return $ r {vroi = roi}
    
localMax r g = do
    mg   <- filterMax32f r g
    mask <- compare32f 2 mg g
    r    <- copyMask32f g mask
    return r

testImage (r,c) = do 
    w <- img 4 1 r c
    set32f 0.0 w (fullroi w)
    set32f 0.5 w $ ROI {r1=50, c1=50, r2 = 250, c2=250}  
    let roi = ROI {r1=100, c1=100, r2 = 200, c2=200}  
    ippiImageJaehne_32f_C1R // dst w roi // checkIPP "ippiSetImageJanehne" [w]
    return w
    
secondOrder image = do
    gx  <- sobelVert image
    gy  <- sobelHoriz image
    gxx <- sobelVert gx
    gyy <- sobelHoriz gy
    gxy <- sobelHoriz gx
    return (gx,gy,gxx,gyy,gxy)    

hessian image = do
    (gx,gy,gxx,gyy,gxy) <- secondOrder image
    ab <- gxx |*| gyy
    cc <- gxy |*| gxy
    h  <- ab  |-| cc
    return h

times 0 f = return
times n f = g where
    g x = do
        v <- f x >>= times (n-1) f
        return v

