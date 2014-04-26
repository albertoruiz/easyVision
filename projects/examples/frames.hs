import Vision.GUI            ( Drawing(Draw), browser, runIt )
import Image.Capture         ( readImages )
import Image.Processing      ( Size(Size), warp, zeroP, saveImage )
import Util.Options          ( optionString, getFlag )
import Util.Homogeneous      ( scaling, desp )
import Numeric.LinearAlgebra ( Contraction((<>)) )
import Text.Printf           ( printf )

--------------------------------------------------------------------------------

transform img k = warp zeroP (Size 480 640) h img
  where
    h = desp (0.002*fromIntegral k-1, -0.2) <> scaling (2*1.002^k) 

--------------------------------------------------------------------------------

save k img = saveImage (printf "frames/frame-%03d.ppm" (k::Int)) img

main = do
    filename <- optionString "--input" "../../data/images/transi/dscn2479.jpg"
    [img] <- readImages [filename]
    let frames = map (transform img) [1..100]

    ok <- getFlag "--save"
    if ok
       then
         sequence_ $ zipWith save [1..] frames
       else
         runIt $  browser "original" [img]    (const Draw)
               >> browser "frames"   frames   (const Draw)

