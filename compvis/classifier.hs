import EasyVision
import Control.Monad(when,(>=>))
import Graphics.UI.GLUT hiding (Size)
import Classifier

machine = distance nearestNeighbour `onP` (const (vector.feat))

featG th sz = lbpN th . resize sz . gray

feat = featG 8 (mpSize 8)

onlyCards sz = onlyRectangles sz (sqrt 2) rgb
               >=> virtualCamera (return . map channelsFromRGB . concat)

main = do
    sz <- findSize

    protos <- getProtos sz channels
    rects <- getFlag "--rectangles"
    let vc = if rects then withChannels >=> onlyCards sz -- same size if we want to save more prototypes
                      else withChannels

    prepare

    (cam,ctrl) <- getCam 0 sz >>= vc >>= withPause

    w <- evWindow (False, protos, fst $ machine protos) "video" sz Nothing  (mouse (kbdcam ctrl))

    launch (worker cam w)

-----------------------------------------------------------------

worker cam w = do

    img <- cam

    let v = feat img

    (click,pats,classify) <- getW w
    when click $ do
        let npats = (img, "?"):pats
            nmach = fst $ machine npats
        putW w (False, npats, nmach)

    inWin w $ do
        drawImage (rgb img)
        pointCoordinates (size (rgb img))
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)
        when (not $ null pats) $ do
            text2D 0.9 0.6 (classify img)

-----------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps,m) <- get st
    st $= (True,ps,m)

mouse _ st (Char 'f') Down _ _ = do
    (_,ps,_) <- get st
    sv <- openYUV4Mpeg (size $ rgb $ fst $ head $ ps) (Just "catalog.yuv") Nothing
    mapM_ (sv.yuv.fst) ps
    writeFile "catalog.labels" $ unlines $ [show n ++"\t"++l | (n,l) <- zip [1..length ps] (map snd ps)]

mouse def _ a b c d = def a b c d

------------------------------------------------------

getProtos sz feat = do
    opt <- getRawOption "--catalog"
    case opt of
        Nothing -> return []
        Just catalog -> do
            readCatalog (catalog++".yuv") sz (catalog++".labels") Nothing feat
