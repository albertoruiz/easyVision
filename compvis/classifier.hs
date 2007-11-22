
import EasyVision
import ImagProc.Ipp.Core
import Control.Monad(when)
import Graphics.UI.GLUT hiding (Point)
import Data.List(minimumBy)

addFeature fun cam = return $ do
    im' <- cam
    let im = modifyROI (shrink (100,200)) im'
    v <- fun im
    return (im, v)

-- normalized lbp histogram
lbpN t im = do
    h <- lbp t im
    let ROI r1 r2 c1 c2 = theROI im
        sc = (256.0::Double) / fromIntegral ((r2-r1-1)*(c2-c1-1))
    return $ map ((*sc).fromIntegral) (tail h)

featLBP sz = addFeature $
    \im -> yuvToGray im >>= resize8u sz >>= lbpN 8


feat = featLBP (mpSize 5)

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= feat >>= withPause

    prepare

    protos <- getProtos feat

    w <- evWindow (False, protos) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    img@(orig,v) <- cam

    (click,pats) <- getW w
    when click $ putW w (False, img:pats)

    inWin w $ do
        drawImage orig
        pointCoordinates (size orig)
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)

    when (not $ null pats) $ inWin r $ do
        let x = minimumBy (compare `on` dist img) pats
            d = dist x img
        drawImage $ fst x
        pointCoordinates (mpSize 5)
        text2D 0.9 0.6 (show $ round d)
        when (d>10) $ do
            setColor 1 0 0
            lineWidth $= 10
            renderPrimitive Lines $ mapM_ vertex $
                [ Point 1 (-1), Point (-1) 1, Point 1 1, Point (-1) (-1) ]

-----------------------------------------------------

dist (_,u) (_,v) = sum $ map (^2) $ zipWith subtract u v

-----------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse _ st (Char 'f') Down _ _ = do
    (_,ps) <- get st
    sv <- openYUV4Mpeg (size $ fst $ head $ ps) (Just "catalog.avi") Nothing
    mapM_ (sv.fst) ps
    writeFile "catalog.labels" $ unlines $ map show [1..length ps]

mouse def _ a b c d = def a b c d

------------------------------------------------------

getProtos feat = do
    opt <- getRawOption "--catalog"
    case opt of
        Nothing -> return []
        Just catalog -> do
            labels <- readFile (catalog ++ ".labels")
            let n = length (lines labels)
            cam <- mplayer (catalog ++ ".avi") (mpSize 20) >>= feat
            sequence (replicate n cam)
