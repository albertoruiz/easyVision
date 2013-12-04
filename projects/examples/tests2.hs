import Vision.GUI.Simple ( Drawing(Draw), browser, runIt )
import Image.Capture
import Image.ROI
import Image.Processing
import Util.Misc(debug)
import Numeric.LinearAlgebra hiding (constant)
import Util.Homogeneous
import Image.Devel
import Data.List(foldl1')


main = do
    ims <- readImages [ "../../data/images/calibration/cube1.png"
                      , "../../data/images/calibration/cube3.png"]
    let im1 = modifyROI (shrink (50,200)) (ims!!0)
        im2 = modifyROI (shrink (100,50)) (ims!!1)
        im3 = absDiff im1 im2
        x1  = rgbToGray im1
        x2  = rgbToGray im2
        x3  = absDiff x1 x2
        
    runIt $ print (simil x1 x2, simil im1 im2) 
         >> browser "tests" [im1,im2,im3] (const Draw)
         >> browser "tests" [x1,x2,x3, sub8u 0 (k 200) (k 100) ] (const Draw)

k = flip constant (Size 100 100)

simil a b = sumPixels d / fromIntegral (roiArea (roi d))
  where
    d = absDiff a b
