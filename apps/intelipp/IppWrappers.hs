{-# OPTIONS -fffi #-}

module IppWrappers where

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
     ippiThreshold_Val_32f_C1R ::  Ptr() -> Int -> Ptr() -> Int -> Double -> Float -> Float -> Int -> IO Int            

foreign import ccall "auxIpp.h ippiSqrt_32f_C1R" 
     ippiSqrt_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Double -> IO Int
    
foreign import ccall "auxIpp.h ippiMinMax_32f_C1R" 
     ippiMinMax_32f_C1R :: Ptr() -> Int -> Double -> Ptr Float -> Ptr Float -> IO Int
    
foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R" 
     warpPerspective32f :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int -> 
                           Int -> Int -> Int -> Int ->  
                           Ptr Double -> Int -> 
                           IO Int

foreign import ccall "auxIpp.h ippErrorMsg" ippError :: Int -> IO ()