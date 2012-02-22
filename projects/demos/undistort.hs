{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

-- remove radial distortion

import Vision.GUI
import ImagProc
import ImagProc.Camera(findSize)

autoParam "RDParam" "rd-"
    [ ("k","Float", realParam (-0.28) (-1) 1)
    , ("mode", "String", stringParam "compute" ["none","table","compute","nn"])]

main = do
    sz <- findSize
    umap <- undistortMap sz 2 (-0.28)    
 
    run $   observe "original" rgb >>> arr grayscale
        >>> withParam (undistort umap)
        >>> observe "undistorted" id

undistort f RDParam{..} x =
    case mode of
        "none"    -> x
        "compute"  -> uradial 2 k x
        "table"    -> remap f InterpLinear x
        "nn"       -> remap f InterpNN x

