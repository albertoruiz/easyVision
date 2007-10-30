-- example of virtual camera

import EasyVision
import System.Environment(getArgs)
import System.IO.Unsafe
import Data.Map as Map hiding (map,size)
import Graphics.UI.GLUT hiding (Size)
import System

-----------------------------------------------------------

interpolate = virtualCamera (return . inter)
    where inter (a:b:rest) = a: (0.5.*a |+| 0.5.*b) :inter (b:rest)

drift alpha = virtualCamera (return . drifter)
    where drifter (a:b:rest) = a : drifter ((alpha .* a |+| (1-alpha).* b):rest)

asFloat grab = return $ grab >>= yuvToGray >>= scale8u32f 0 1

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = fromList $ zip args (tail args)
        sz   = findSize args

    let alpha = read $ findWithDefault "0.9" "--alpha" opts

    state <- prepare ()

    (cam,ctrl) <- mplayer (args!!0) sz
                  >>= monitorizeIn "original" (Size 150 200) id state
                  >>= asFloat
                  >>= drift alpha >>= interpolate
                  >>= withPause

    addWindow "interpolate" sz Nothing  (const (kbdcam ctrl)) state

    launch state (worker cam)

-----------------------------------------------------------------

worker cam inWindow _ = do

    inWindow "interpolate" $ do
        cam >>= drawImage















