-- save captured video
-- then you can convert the generated yuv to a nicer format:
-- $ ./justsave webcam1 --save file.yuv
-- $ mencoder file.yuv -o file.avi -flip -ovc lavc -fps 15

import EasyVision
import System.Environment(getArgs)
import Data.Map as Map hiding (map,size)

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = findSize args

    (cam,ctrl) <- mplayer (args!!0) sz >>= withPause

    state <- prepare ()

    addWindow "image" sz Nothing  (const (kbdcam ctrl)) state

    sv <- openYUV4Mpeg sz (Map.lookup "--save" opts)
                          (read `fmap` Map.lookup "--limit" opts)

    launch state (worker cam sv)

-----------------------------------------------------------------

worker cam save inWindow _ = do

    inWindow "image" $ do
        orig <- cam
        drawImage orig
        save orig
        return ()
