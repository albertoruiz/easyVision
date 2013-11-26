import Vision.GUI.Simple ( Drawing(Draw), browser, runIt )
import Image.Capture ( readImages )
import Image.Core


main = do
    [img] <- readImages [ "../../data/images/calibration/cube3.png" ]
    runIt $ browser "test" [f img, img] (const Draw)


stModifyPix f x p = stRead x p >>= stWrite x p . f

h (Word24 r g b) = Word24 (255-r) 0 (255-b)


f :: Image Word24 -> Image Word24
f x = runSTImage $ do
    let rs = [100..200]
        cs = [150..250]
    z <- thawImage x
    sequence_ [ stModifyPix h z (Pixel i j) | i<-rs, j <- cs]
    sequence_ [ stWrite z (Pixel (i+50) (j+200)) (Word24 (fromIntegral i) 0 0) | i <- rs, j<-rs ]
    -- stWrite z (Pixel 10000 10000) (Word24 0 0 0)
    return z

