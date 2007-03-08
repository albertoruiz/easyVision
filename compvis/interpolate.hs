-- example of virtual camera

import EasyVision
import System.Environment(getArgs)
import System.IO.Unsafe
import Data.Map as Map hiding (map,size)
import Graphics.UI.GLUT hiding (Size)
import System

-----------------------------------------------------------

interpolate = virtualCamera (return . inter)
    where
        inter (a:b:rest) = a: (a.+.b) :inter (b:rest)
            where a .+. b = unsafePerformIO $ do
                                a' <- scale32f 0.5 a
                                b' <- scale32f 0.5 b
                                c <- a' |+| b'
                                return c

drift alpha = virtualCamera (return . drifter)
    where
        drifter (a:b:rest) = an : drifter (bn:rest)
            where (an, bn) = unsafePerformIO $ do
                                a' <- scale32f alpha a
                                b' <- scale32f (1-alpha) b
                                an <- a' |+| b'
                                return (a',an)


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
                  >>= asFloat >>=  drift alpha >>= interpolate 
                  >>= withPause

    state <- prepare ()

    addWindow "interpolate" sz Nothing  (const (kbdcam ctrl)) state

    launch state (worker cam)

-----------------------------------------------------------------

worker cam inWindow _ = do

    inWindow "interpolate" $ do
        cam >>= drawImage














