-- save captured video

import EasyVision
import System.Environment(getArgs)
import System.IO.Unsafe
import Data.Map as Map hiding (map,size)
import Graphics.UI.GLUT hiding (Size)
import System


main = do
    args <- getArgs

    let opts = fromList $ zip args (tail args)

    let sz = if member "--size" opts
                 then mpSize $ read $ findWithDefault "20" "--size" opts
                 else Size (read $ findWithDefault "480" "--rows" opts)
                           (read $ findWithDefault "640" "--cols" opts)


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













