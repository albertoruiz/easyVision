{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, RecordWildCards #-}

module OpenCV.MSER(
  mserRaw,
  MSERParams(..)
) where

import Image.Develop
import ImagProc.Generic
import Foreign
import Vision.GUI.Parameters

------------------------------------------------------------------

$(autoParam "MSERParams" ""
        [ ("delta"        , "Int"   ,    intParam    30  0 255  )
        , ("minArea"      , "Int",        percent    5          )
        , ("maxArea"      , "Int",        percent    10         )
        , ("maxVariation" , "Int",        percent    50         )
        , ("minDiversity" , "Int",        percent    80         )
--        , ("maxEvolution" , "Int"   ,    intParam   200 0  1000 )
--        , ("areaThreshold", "Double",   realParam    20 0  100  )
--        , ("minMargin"    , "Double",   realParam   0.003 0 0.1 )
--        , ("edgeBlurSize" , "Int"   ,    intParam   1   0 10    )
        ]
 )

------------------------------------------------------------------

mserRaw :: MSERParams -> ImageGray -> ImageGray
mserRaw MSERParams {..} im@(G s) = unsafePerformIO $ do
    let Size r c = size im
        area = r*c
        minArea' = minArea * area `div` 10000
        maxArea' = maxArea * area `div` 100
    res <- clone im
    app1G (c_testMSER (fi delta) (fi minArea') (fi maxArea')
                      (fromIntegral maxVariation / 100) (fromIntegral minDiversity / 100)
                      -- (fi maxEvolution) areaThreshold minMargin (fi edgeBlurSize))
                      0 0 0 0)
          res
    touchForeignPtr (fptr s)
    return res

foreign import ccall "testMSER"
    c_testMSER :: CInt -> CInt -> CInt -> Double -> Double -> CInt -> Double -> Double -> CInt
               -> Ptr () -> CInt -> CInt -> CInt
               -> CInt -> CInt -> CInt -> CInt
               -> IO ()

