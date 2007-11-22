-- save captured video
-- then you can convert the generated yuv to a nicer format:
-- $ ./justsave webcam1 --save file.yuv
-- $ mencoder file.yuv -o file.avi -ovc lavc -fps 15

import EasyVision
import System.Environment(getArgs)
import Data.Map as Map hiding (map,size)

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withPause

    prepare

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))

    filename <- getRawOption "--save"
    limit    <- maybeOption "limit"
    sv <- openYUV4Mpeg sz filename limit
    -- this is the same as optionalSaver in EasyVision.Util

    launch (worker cam sv w)

-----------------------------------------------------------------

worker cam save w = do

    inWin w $ do
        orig <- cam
        drawImage orig
        save orig
