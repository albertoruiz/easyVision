import Vision.GUI
import Image.Processing
import Image.Capture


main = do
    [base] <- readImages ["../../data/images/calibration/cube3.png"]
    run $ getBackground
        >>> observe "template" (rgb.snd)
        >>> observe "image" (sh base)


getBackground = clickKeep "click to set template" f g Nothing
  where
    f r = id
    g = Draw . rgb . fst

sh b (x,t) = copyMask b (rgb x) mask
  where
    mask = uvdif x t .>. 20

uvdif :: Channels -> Channels -> Image I8u
uvdif b x = resize (size (rgb x)) $ add8u 1 (dc uCh x b) (dc vCh x b)
  where
    dc c = absDiff `on` c

