{-# OPTIONS -fffi #-}

import Ipp
 
import Foreign 
import Foreign.C.Types
 
foreign import ccall "ippiImageJaehne_32f_C1R" 
  ippiImageJaehne_32f_C1R :: Ptr() -> Int -> Double -> IO Int

foreign import ccall "auxIpp.h ippiSet_32f_C1R" 
  ippiSet_32f_C1R :: CFloat -> Ptr() -> Int -> Double -> IO Int
 
foreign import ccall "auxIpp.h ippiFilterGauss_32f_C1R" 
     ippiFilterGauss_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> Int -> IO Int
     
foreign import ccall "auxIpp.h ippiCopy_32f_C1R" 
     ippiCopy_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
      

testMalloc = do
    imgs <- mapM (\r -> img 4 1 r r) [300 .. 400]
    print $ map step imgs    
    
    
testImage = do 
    w <- img 4 1 300 300
    mK1 (ippiSet_32f_C1R 0.0) (fullroi w) w
    let roi = ROI {r1=50, c1=50, r2 = 250, c2=250}  
    mK1 (ippiSet_32f_C1R 0.5) roi w
    let roi = ROI {r1=100, c1=100, r2 = 200, c2=200}  
    mK1 ippiImageJaehne_32f_C1R roi w
    return w
    
copy32f im = cre im fun where
    fun im r = mK2 ippiCopy_32f_C1R (fullroi im) r im

cre im f = do
    r <- img (datasize im) (layers im) (rows im) (cols im)
    f im r
    return r     
    
main = do
    --loop 1000 testMalloc
  
    w <- testImage
    imageshow w 
    
    --d <- img 4 1 300 300
    --mK2 ippiCopy_32f_C1R (fullroi d) d w
    d <- copy32f w
    
    let roi = ROI {r1=150, c1=150, r2 = 299, c2=299}
    mK2p1 ippiFilterGauss_32f_C1R 55 roi d w
    imageshow d
    
