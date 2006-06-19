module Typical where

import Ipp
     
set32f v im roi = ippiSet_32f_C1R v // dst im roi // checkIPP "ippSet" [im]

scale8u32f vmin vmax im = do
    r <- img 4 1 (height im) (width im)
    let roi = fullroi r
    (ippiScale_8u32f_C1R // src im roi // dst r roi) vmin vmax // checkIPP "ippiScale_8u32f_C1R" [r,im]
    return r
       
simplefun_1 ippfun roifun msg = g where
    g im = do
        r <- imgAs im
        let roi = roifun (fullroi r)
        ippfun // src im roi // dst r roi // checkIPP msg [r,im]
        return r

copy32f = simplefun_1 ippiCopy_32f_C1R id "ippiCopy_32f_C1R"
abs32f  = simplefun_1 ippiAbs_32f_C1R  id "ippiAbs_32f_C1R"    
sqrt32f = simplefun_1 ippiSqrt_32f_C1R id "ippiSqrt_32f_C1R" 
filterSobelVert = simplefun_1 ippiFilterSobelVert_32f_C1R (shrink (1,1)) "ippiFilterSobelVert_32f_C1R"
filterSobelHoriz = simplefun_1 ippiFilterSobelHoriz_32f_C1R (shrink (1,1)) "ippiFilterSobelHoriz_32f_C1R"

gauss mask im = do
    r <- imgAs im
    let roi = shrink (1,1) (fullroi r)
    (ippiFilterGauss_32f_C1R // src im roi // dst r roi) mask // checkIPP "ippiFilterGauss_32f_C1R" [r,im]
    return r

thresholdVal32f t v code im = do
    r <- imgAs im
    let roi = (fullroi r)
    (ippiThreshold_Val_32f_C1R // src im roi // dst r roi) t v code // checkIPP "ippiThreshold_Val_32f_C1R" [r,im]
    return r

filterMax32f sz im = do
    r <- imgAs im
    let d = (sz-1) `quot` 2
    let roi = shrink (d,d) (fullroi r)
    (ippiFilterMax_32f_C1R // src im roi // dst r roi) (ippRect sz sz) (ippRect d d) // checkIPP "ippiFilterMax_32f_C1R" [r,im]
    return r

simplefun_2 ippfun roifun msg = g where
    g im1 im2 = do
        r <- imgAs im1
        let roi = roifun (fullroi r)
        ippfun // src im1 roi // src im2 roi // dst r roi // checkIPP msg [r,im1, im2]
        return r

add32f = simplefun_2 ippiAdd_32f_C1R id "ippiAdd_32f_C1R"
mul32f = simplefun_2 ippiMul_32f_C1R id "ippiMul_32f_C1R"
sub32f = flip $ simplefun_2 ippiSub_32f_C1R id "ippiSub_32f_C1R" -- more natural argument order

infixl 7  |*|
infixl 6  |+|, |-|
(|*|) = mul32f
(|+|) = add32f
(|-|) = sub32f


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
