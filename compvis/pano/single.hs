-- panoramic with a single camera.
-- Click on the image to get the base view and 
-- on the autopanoramic window to start the optimization

import EasyVision
import Graphics.UI.GLUT hiding (Size, Point, Matrix)
import Util.Options

main = do
    sz <- findSize

    f <- getOption "--focal" 2.8

    --(cam,ctrl) <- getCam 0 sz >>= withChannels >>= withPause
    (cam,ctrl) <- getCam 0 sz ~> channels >>= addDist >>= withPause

    prepare

    w  <- evWindow Nothing "image" sz Nothing  (mouse (kbdcam ctrl))
    --wd <- evWindow Nothing "diff"  sz Nothing  (mouse (kbdcam ctrl))

    let vcam = do
        st <- getW w
        case st of
            Nothing -> do orig <- cam
                          putW w (Just orig)
            _       -> return ()
        Just i <- getW w
        return i

{-
    p <- panoramic (mpSize 5) 2.8 2.8 2.0 vcam cam (float.grayscale)
                                                   (float . resize (mpSize 5) . grayscale)
                                                   (float . resize (mpSize 5) . grayscale)
                                                   (\a b -> sum32f (abs32f (a |-| b)))

-}

    p <- panoramic (mpSize 5) f f 2.0 vcam cam (float.grayscale.fst)
                                                   (fst.snd)
                                                   (snd.snd)
                                                   (\d e -> sum32f (d |*| e))


    launch (worker vcam cam w  p)

-----------------------------------------------------------------

edgedist img = (r, float edges) where
    th = 0.3
    smooth = 2
    gsmooth = smooth `times` gauss Mask5x5 $ float $ resize (mpSize 10) $ grayscale $ img
    edges = canny (th/3,th) . gradients $ gsmooth
    r = distanceTransform [1,1.4,2.2] (notI $ edges)


addDist cam = return $ do
    orig <- cam
    let (r,e) = edgedist orig
    return (orig, (r, e))


worker vcam cam w  p = do
    inWin w $ do
        p >>= drawImage


-------------------------------------------------

times n f = (!!n) . iterate f

mouse _ st (MouseButton LeftButton) Down _ _ = do
    putW st Nothing

mouse def _ a b c d = def a b c d

-------------------------------------------------------
