import Vision.GUI
import Image.Processing
import Image.Capture
import Util.Homogeneous
import Numeric.LinearAlgebra


main = do
    base <- loadRGB "../../data/images/transi/dscn2070.jpg"
    run $ getBackground
        >>> observe "template" (rgb.snd)
        >>> arrL (zip [0..])
        >>> arr (\(k,(x,t)) -> (f k base, x, t))
        >>> observe "image" sh

f k = warp zeroP (Size 480 640) h
  where
    h = desp (0.0002*fromIntegral k-1 , 0) <> scaling (2*1.0002^k)


getBackground = clickKeep "click to set template" f g Nothing
  where
    f r = id
    g = Draw . rgb . fst

sh (b,x,t) = copyMask b (rgb x) mask
  where
    mask = uvdif x t .>. 20

uvdif :: Channels -> Channels -> Image I8u
uvdif b x = resize (size (rgb x)) $ add8u 1 (dc uCh x b) (dc vCh x b)
  where
    dc c = absDiff `on` c

