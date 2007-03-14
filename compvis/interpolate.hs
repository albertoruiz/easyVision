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

    let sz = if member "--size" opts
                 then mpSize $ read $ findWithDefault "20" "--size" opts
                 else Size (read $ findWithDefault "480" "--rows" opts)
                           (read $ findWithDefault "640" "--cols" opts)

    let alpha = read $ findWithDefault "0.9" "--alpha" opts

    (cam,ctrl) <- mplayer (args!!0) sz
                  >>= asFloat
                  >>= drift alpha >>= interpolate 
                  >>= withPause

    state <- prepare ()

    addWindow "interpolate" sz Nothing  (const (kbdcam ctrl)) state

    launch state (worker cam)

-----------------------------------------------------------------

worker cam inWindow _ = do

    inWindow "interpolate" $ do
        cam >>= drawImage














