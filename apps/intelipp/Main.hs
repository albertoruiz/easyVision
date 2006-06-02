{-# OPTIONS -fffi #-}

import Ipp
import Draw
 
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
            
foreign import ccall "auxIpp.h ippiFilterMax_32f_C1R" 
     ippiFilterMax_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Double -> Double -> IO Int
            
foreign import ccall "auxIpp.h ippiCompare_32f_C1R" 
     ippiCompare_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> Double -> Int -> IO Int
            
            
------------------------------------------------------      
foreign import ccall "auxIpp.h mycvOpenCamera"
     openCamera :: Ptr CChar -> IO Int

foreign import ccall "auxIpp.h mycvSetModeCamera"
     setCameraMode :: Int -> Int -> Int -> Int -> IO()

foreign import ccall "auxIpp.h mycvGetFrameCamera"
     getFrame :: Int -> Ptr Int -> IO (Ptr CChar)
-------------------------------------------------------

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
    fun im r = mK2p2 ippiScale_8u32f_C1R 0.0 1.0 (fullroi im) r im

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

gauss mask im = cre im fun where
    roi = shrink (1,1) (fullroi im)
    fun im r = mK2p1 ippiFilterGauss_32f_C1R mask roi r im

filterMax32f sz im = cre im fun where
    d = (sz-1) `quot` 2
    roi = shrink (d,d) (fullroi im)
    fun im r = mK2p2 ippiFilterMax_32f_C1R (encodeAsDouble sz sz) (encodeAsDouble d d) roi r im

compare32f code im1 im2 = cre2' im1 im2 fun 1 1 where
    fun im1 im2 r = mK3p1 ippiCompare_32f_C1R code (fullroi im1) r im1 im2



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
    
    
grab c = do
    pstep <- malloc
    dat <- getFrame c pstep
    stepc <- peek pstep
    res <- img 1 1 288 384
    copyArray (castPtr $ ptr res) dat (288*stepc)
    return res 
    

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

    
main = do
    args <- getArgs
    filename <- newCString (args!!0)
    cam <- openCamera filename
    setCameraMode cam 1 288 384
    imageShow (384,288) (visor cam)
    
    
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
    