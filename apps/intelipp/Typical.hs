module Typical where

import Ipp
     
imgAsR1 roifun im = do 
    r <- imgAs im
    return r {vroi = roifun (vroi im)} 

cr1 f im r = f // src im (vroi r) // dst r (vroi r)

imgAsR2 roifun im1 im2 = do 
    r <- imgAs im1
    return r {vroi = roifun (vroi im1) (vroi im2)}

cr2 f im1 im2 r = f // src im1 (vroi r) // src im2 (vroi r)// dst r (vroi r)

----------------------------------------

gauss mask im = do
    r <- imgAsR1 (shrink (s,s)) im
    (cr1 ippiFilterGauss_32f_C1R im r) mask // checkIPP "gauss" [im]
    return r
  where s = case mask of
                33 -> 1
                55 -> 2
                _  -> error "illegal mask"
     
set32f v im roi = ippiSet_32f_C1R v // dst im roi // checkIPP "set32f" [im]

scale8u32f vmin vmax im = do
    r' <- img 4 1 (height im) (width im)
    let r = r' {vroi = vroi im}
    (cr1 ippiScale_8u32f_C1R im r) vmin vmax // checkIPP "scale8u32f" [im]
    return r
       
thresholdVal32f t v code im = do
    r <- imgAsR1 id im
    (cr1 ippiThreshold_Val_32f_C1R im r) t v code // checkIPP "thresholdVal32f" [im]
    return r

filterMax32f sz im = do
    let d = (sz-1) `quot` 2
    r <- imgAsR1 (shrink (d,d)) im
    (cr1 ippiFilterMax_32f_C1R im r) (ippRect sz sz) (ippRect d d) // checkIPP "filterMax32f" [im]
    return r       
       
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
 
copyROI32f r im = ippiCopy_32f_C1R // src im (vroi im) // dst r (vroi im) // checkIPP "copyROI32f" [im]

---------------------------------------------

simplefun2 ippfun roifun msg = g where
    g im1 im2 = do
        r <- imgAsR2 roifun im1 im2
        cr2 ippfun im1 im2 r // checkIPP msg [im1,im2]
        return r

fr1 r1 r2 = r1

infixl 7  |*|
infixl 6  |+|, |-|
(|*|) = simplefun2 ippiMul_32f_C1R fr1 "mul32f"
(|+|) = simplefun2 ippiAdd_32f_C1R fr1 "add32f"
(|-|) = flip $ simplefun2 ippiSub_32f_C1R fr1 "sub32f" -- more natural argument order


compare32f code im1 im2 = do
    r <- img 1 1 (height im1) (width im1)
    let roi = (fullroi im1)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) code // checkIPP "compare32f" [r,im1,im2]
    return r 

copyMask32f im mask = do
    r <- imgAs im
    let roi = fullroi im
    set32f 0.0 r roi
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [r,im,mask]
    return r
    
localMax g = do
    mg   <- filterMax32f 3 g
    mask <- compare32f 2 mg g
    r    <- copyMask32f g mask
    return r
