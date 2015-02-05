-- generated automatically by adapter.hs

module Image.Processing.IPP.Auto where

import Image.Processing.IPP.AutoGen
import Image.Processing.IPP.Adapt

-------- arity 0 -------------

-------- arity 1 -------------

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C1R  = {-# SCC "ippiCopy_8u_C1R" #-} auto_1_8u_C1R f "ippiCopy_8u_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C1R pSrc srcStep pDst dstStep roiSize

{- |     Converts an RGB image to gray scale (fixed coefficients) -}
ioRGBToGray_8u_C3C1R  = {-# SCC "ippiRGBToGray_8u_C3C1R" #-} auto_1_8u_C3C1R f "ippiRGBToGray_8u_C3C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiRGBToGray_8u_C3C1R pSrc srcStep pDst dstStep roiSize


------ arity 2 -------------


------ inplace arity 2 ------


----------------------------
