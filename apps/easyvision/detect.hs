-- example of virtual camera

import Ipp
import System.Environment(getArgs)
import System.IO.Unsafe
import Data.Map hiding (map,size)

dropFrames n (h:t) = h : dropFrames n (drop n t)

rever ims = (a++b++a++rever c) where
    a = take 40 ims
    b = reverse a
    c = drop 40 ims

detectMov cond ((a,f):(b,g):t) =
    if cond (absdif f g)
        then (a,f) : detectMov cond ((b,g):t)
        else detectMov cond ((b,g):t)

absdif a b = unsafePerformIO $ absDiff8u a b >>= sum8u -- hmm

addSmall sz grab = return $ do
    im <- grab
    f <- yuvToGray im >>= resize8u sz
    return (im,f)

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
                  >>= virtualCamera (return . detectMov (th*255*90*120<))
                  >>= withPause

    state <- prepare undefined ()

    addWindow "movement" sz Nothing  (const (kbdcam ctrl)) state

    launch state (worker cam)

-----------------------------------------------------------------

worker cam inWindow _ _ = do

    inWindow "movement" $ do
        cam >>= return . fst >>= yuvToRGB >>= drawImage
