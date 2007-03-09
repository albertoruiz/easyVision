-- example of virtual camera

import EasyVision
import System.Environment(getArgs)
import System.IO.Unsafe
import Data.Map as Map hiding (map,size)
import Graphics.UI.GLUT hiding (Size)
import System

dropFrames n (h:t) = h : dropFrames n (drop n t)

rever ims = (a++b++a++rever c) where
    a = take 40 ims
    b = reverse a
    c = drop 40 ims

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = fromList $ zip args (tail args)

    let sz = if member "--size" opts
                 then mpSize $ read $ findWithDefault "20" "--size" opts
                 else Size (read $ findWithDefault "480" "--rows" opts)
                           (read $ findWithDefault "640" "--cols" opts)

    let th = read $ findWithDefault "0.01" "--sensi" opts

    (cam,ctrl) <- mplayer (args!!0) sz
                  >>= addSmall (Size 90 120)
                  >>= detectMov (th*255*90*120<)
                  >>= withPause

    state <- prepare ()

    addWindow "motion" sz Nothing  (const (kbdcam ctrl)) state

    sv <- openYUV4Mpeg sz (Map.lookup "--save" opts)
                          (read `fmap` Map.lookup "--limit" opts)

    launch state (worker cam sv)

-----------------------------------------------------------------

worker cam save inWindow _ = do

    inWindow "motion" $ do
        orig <- cam >>= return . fst
        yuvToRGB orig >>= drawImage
        system "artsplay /usr/share/sounds/KDE_Notify.wav"
        save orig
        windowStatus $= Shown
        return ()













