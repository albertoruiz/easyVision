-- example of virtual cameras

import EasyVision
import Util.Options

-----------------------------------------------------------

interpolate = virtualCamera inter
    where inter (a:b:rest) = a: (0.5.*a |+| 0.5.*b) :inter (b:rest)

drift alpha = virtualCamera drifter
    where drifter (a:b:rest) = a : drifter ((alpha .* a |+| (1-alpha).* b):rest)

asFloat grab = return $ return . float . yuvToGray =<< grab

------------------------------------------------------------

main = do

    sz <- findSize

    alpha <- getOption "--alpha" 0.9

    prepare

    (cam,ctrl) <- getCam 0 sz
                  >>= monitor "original" (Size 150 200) drawImage
                  >>= asFloat
                  >>= drift alpha >>= interpolate
                  >>= withPause

    w <- evWindow () "interpolate" sz Nothing  (const (kbdcam ctrl))

    launch $ inWin w $ cam >>= drawImage
