{-# OPTIONS -fffi #-}

import Ipp
import Draw
import Camera 
 
import Foreign 
import Foreign.C.Types
import Foreign.C.String(newCString)
import System.Environment(getArgs)
 
foreign import ccall "ippiImageJaehne_32f_C1R" 
  ippiImageJaehne_32f_C1R :: Ptr() -> Int -> Double -> IO Int

foreign import ccall "auxIpp.h ippiSet_32f_C1R" 
  ippiSet_32f_C1R :: CFloat -> Ptr() -> Int -> Double -> IO Int
 
foreign import ccall "auxIpp.h ippiFilterGauss_32f_C1R" 
     ippiFilterGauss_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Int -> IO Int
     
foreign import ccall "auxIpp.h ippiCopy_32f_C1R" 
     ippiCopy_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      
foreign import ccall "auxIpp.h ippiCopy_32f_C1MR" 
     ippiCopy_32f_C1MR :: Ptr() -> Int -> Ptr() -> Int -> Double -> Ptr() -> Int -> IO Int
            
foreign import ccall "auxIpp.h ippiCopy_8u_C1R" 
     ippiCopy_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      
foreign import ccall "auxIpp.h ippiScale_32f8u_C1R" 
     ippiScale_32f8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Float -> Float -> IO Int
      
foreign import ccall "auxIpp.h ippiScale_8u32f_C1R" 
     ippiScale_8u32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Float -> Float -> IO Int
            
foreign import ccall "auxIpp.h ippiFilterSobelVert_32f_C1R" 
     ippiFilterSobelVert_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      
foreign import ccall "auxIpp.h ippiFilterSobelHoriz_32f_C1R" 
     ippiFilterSobelHoriz_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      
foreign import ccall "auxIpp.h ippiAbs_32f_C1R" 
     ippiAbs_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      
foreign import ccall "auxIpp.h ippiAdd_32f_C1R" 
     ippiAdd_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
            
foreign import ccall "auxIpp.h ippiSub_32f_C1R" 
     ippiSub_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
            
foreign import ccall "auxIpp.h ippiMul_32f_C1R" 
     ippiMul_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
            
            
foreign import ccall "auxIpp.h ippiFilterMax_32f_C1R" 
     ippiFilterMax_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Double -> Double -> IO Int
            
foreign import ccall "auxIpp.h ippiCompare_32f_C1R" 
     ippiCompare_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> Double -> Int -> IO Int
            
foreign import ccall "auxIpp.h ippiThreshold_Val_32f_C1R" 
     ippiThreshold_Val_32f_C1R ::  Ptr() -> Int -> Ptr() -> Int -> Double -> Double -> Double -> Int -> IO Int            


testMalloc = do
    imgs <- mapM (\r -> img 4 1 r r) [300 .. 400]
    print $ map step imgs    
    
    
testImage (r,c) = do 
    w <- img 4 1 r c
    mK1 (ippiSet_32f_C1R 0.0) (fullroi w) w
    let roi = ROI {r1=50, c1=50, r2 = 250, c2=250}  
    mK1 (ippiSet_32f_C1R 0.5) roi w
    let roi = ROI {r1=100, c1=100, r2 = 200, c2=200}  
    mK1 ippiImageJaehne_32f_C1R roi w
    return w
    
copy32f im = cre im fun where
    fun im r = mK2 ippiCopy_32f_C1R (fullroi im) r im

scale8u32f vmin vmax im = cre' im fun 4 1 where
    fun im r = mK2p2 ippiScale_8u32f_C1R vmin vmax (fullroi im) r im

filterSobelVert im = cre im fun where
    roi = shrink (1,1) (fullroi im)
    fun im r = mK2 ippiFilterSobelVert_32f_C1R roi r im

filterSobelHoriz im = cre im fun where
    roi = shrink (1,1) (fullroi im)
    fun im r = mK2 ippiFilterSobelHoriz_32f_C1R roi r im

abs32f im = cre im fun where
    fun im r = mK2 ippiAbs_32f_C1R (fullroi im) r im

add32f im1 im2 = cre2 im1 im2 fun where
    fun im1 im2 r = mK3 ippiAdd_32f_C1R (fullroi im1) r im1 im2

sub32f im1 im2 = cre2 im1 im2 fun where
    fun im1 im2 r = mK3 ippiSub_32f_C1R (fullroi im1) r im1 im2

mul32f im1 im2 = cre2 im1 im2 fun where
    fun im1 im2 r = mK3 ippiMul_32f_C1R (fullroi im1) r im1 im2

thresholdVal32f t v code im = cre im fun where
    fun im r = mK2p3 ippiThreshold_Val_32f_C1R t v code (fullroi im) r im

infixl 7  |*|
(|*|) = mul32f

infixl 6  |+|, |-|
(|+|) = add32f
(|-|) = sub32f


gauss mask im = cre im fun where
    roi = shrink (1,1) (fullroi im)
    fun im r = mK2p1 ippiFilterGauss_32f_C1R mask roi r im

filterMax32f sz im = cre im fun where
    d = (sz-1) `quot` 2
    roi = shrink (d,d) (fullroi im)
    fun im r = mK2p2 ippiFilterMax_32f_C1R (encodeAsDouble sz sz) (encodeAsDouble d d) roi r im


compare32f code im1 im2 = do
    r <- img 1 1 (rows im1) (cols im1)
    let roi = (fullroi im1)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) code // checkIPP "compare32f" [r,im1,im2]
    return r 


copyMask32f im mask = do
    r <- imgAs im
    let roi = fullroi im
    ippiSet_32f_C1R 0.0 // dst r roi // checkIPP "set32f" [r]
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [r,im,mask]
    return r
    

cre im f = do
    r <- img (datasize im) (layers im) (rows im) (cols im)
    f im r
    return r     
    
cre' im f d l = do
    r <- img d l (rows im) (cols im)
    f im r
    return r    
    
cre2 im1 im2 f = do
    r <- img (datasize im1) (layers im1) (rows im1) (cols im1)
    f im1 im2 r
    return r     
    
cre2' im1 im2 f d l = do
    r <- img d l (rows im1) (cols im1)
    f im1 im2 r
    return r     
    
    
pyr im k = do
    let roi = ROI {r1=150, c1=20, r2 = 290, c2=290}  
    --r <- img (datasize im) (layers im) (rows im) (cols im)
    r <- copy32f im
    mK2p1 ippiFilterGauss_32f_C1R 33 roi r im --3x3 mask (or 55 (5x5))
    mK2 ippiCopy_32f_C1R (fullroi im) im r
    if k == 1000 then error "OK"
                else return im
    
    
main'' = do
    w <- testImage (300,500)
    imageShow (300,300) (pyr w) 
    
    
secondOrder image = do
    gx  <- filterSobelVert image
    gy  <- filterSobelHoriz image
    gxx <- filterSobelVert gx
    gyy <- filterSobelHoriz gy
    gxy <- filterSobelHoriz gx
    return (gx,gy,gxx,gyy,gxy)
     
hessian image = do
    (gx,gy,gxx,gyy,gxy) <- secondOrder image
    ab <- gxx |*| gyy
    cc <- gxy |*| gxy
    h  <- ab  |-| cc
    return h

localMax g = do
    mg   <- filterMax32f 3 g
    mask <- compare32f 2 mg g
    r    <- copyMask32f g mask
    return r

visor cam k = do
    im  <- grab cam
    imf <- scale8u32f 0 1 im  >>= gauss 55
    gx  <- filterSobelVert imf
    gy  <- filterSobelHoriz imf
    agx <- abs32f gx
    agy <- abs32f gy
    g   <- add32f agx agy >>= gauss 55
    mg  <- filterMax32f 3 g >>= filterMax32f 3
    lm  <- compare32f 2 mg g
    return mg


visor' cam k = do
    im  <- grab cam
    imf <- scale8u32f 0 0.5 im >>= gauss 55 >>= gauss 55 
    h   <- hessian imf  >>= localMax >>= thresholdVal32f 0.3 0.0 0 >>= thresholdVal32f 0.3 1 4
    return h
    
    
    
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) 1 (288,384)
    imageShow (w,h) (visor' cam)
    
    
main' = do
    --loop 1000 testMalloc
  
    w <- testImage (300,300)
    --imageShow' (300,300) (const w) 
    
    --d <- img 4 1 300 300
    --mK2 ippiCopy_32f_C1R (fullroi d) d w
    d <- copy32f w
    
    let roi = ROI {r1=150, c1=150, r2 = 299, c2=299}
    mK2p1 ippiFilterGauss_32f_C1R 55 roi d w
    imageShow' (300,300) (const d) 
    