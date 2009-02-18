-- shows --tot [250] frames and exits.
-- Useful for grab speed measurements.
-- $ time mplayer /home/brutus/space/data/videos/poster1.avi -benchmark -frames 500 -nosound
-- $ time ./benchmark 'poster1.avi -benchmark' --rows=576 --cols=720 --tot=500
-- (with the original video size to avoid rescaling)
-- we get similar results, about 3.5s (>140fps, ~7ms)

import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage'.f)
run c = prepare >> (c >>= launch . (>> return ()))

main = do
    sz <- findSize
    n <- getOption "--tot" 250

    run $ camera >>= observe "Video" gray >>= countDown n
