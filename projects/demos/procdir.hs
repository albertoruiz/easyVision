-- $ ./procdir --sphotos=../../data/images/calibration [--pad=3] [--output=result/img-]

import Vision.GUI
import ImagProc
import Util.Options

--------------------------------------------------------------------------------

f = uradial 2 (-0.28) 

--------------------------------------------------------------------------------

main = do
    pad <- getOption "--pad" 3
    outputPath <- optionString "--output" "img-"
    prepare
    runNT camera
        $   observe "Image" rgb 
        >>> arr (f . grayscale) 
        >>> arrL (zip [0..])
        >>> arrIO (save pad outputPath)

--------------------------------------------------------------------------------

save npad dst (k,img) = saveGray (dst ++ showpad npad k ++ ".png") img
  where
    showpad n k = replicate (n-length sk) '0' ++ sk
      where
        sk = show k

