import EasyVision
import Graphics.UI.GLUT hiding (Size, Point, Matrix)
import Util.Options

main = do
    sz  <- findSize

    f <- getOption "--focal" 2.8

    prepare

    (cam0,ctrl0) <- getCam 0 sz ~> channels >>= monitor "cam0" (mpSize 5) (drawImage.grayscale) >>= addDist >>= withPause
    (cam1,ctrl1) <- getCam 1 sz ~> channels >>= monitor "cam1" (mpSize 5) (drawImage.grayscale) >>= addDist >>= withPause




{-
    p <- panoramic (mpSize 5) 2.8 2.8 2.0 cam0 cam1 (float.grayscale)
                                                   (float . resize (mpSize 5) . grayscale)
                                                   (float . resize (mpSize 5) . grayscale)
                                                   (\a b -> sum32f (abs32f (a |-| b)))
-}


    p <- panoramic (mpSize 20) f f 2.0 cam0 cam1   (float.grayscale.fst)
                                                   (fst.snd)
                                                   (snd.snd)
                                                   (\d e -> sum32f (d |*| e))

    launch (worker p)

-----------------------------------------------------------------

edgedist img = (r, float edges) where
    th = 0.5
    smooth = 2
    gsmooth = smooth `times` gauss Mask5x5 $ float $ resize (mpSize 10) $ grayscale $ img
    edges = canny (th/3,th) . gradients $ gsmooth
    r = distanceTransform [1,1.4,2.2] (notI $ edges)


addDist cam = return $ do
    orig <- cam
    let (r,e) = edgedist orig
    return (orig, (r, e))


worker  p = do
    p
    return ()


-------------------------------------------------

times n f = (!!n) . iterate f

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= Nothing

mouse def _ a b c d = def a b c d

-------------------------------------------------------
