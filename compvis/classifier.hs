
import EasyVision
import ImagProc.Ipp.Core
import Control.Monad(when,(>=>))
import Graphics.UI.GLUT hiding (Point,Size)
import Data.List(minimumBy, sortBy)
import Classifier

machine = distance nearestNeighbour `onP` (const (vector.feat))

featG th sz = lbpN th . resize sz . fromYUV

feat = featG 8 (mpSize 8)

main = do
    sz <- findSize

    protos <- getProtos sz id

    prepare

    (cam,ctrl) <- getCam 0 sz
--                  >>= onlyRectangles (mpSize 10) (sqrt 2)
--                  >>= virtualCamera (return . concat)
--                  >>= virtualCamera (return . map (toYUV :: ImageRGB -> ImageYUV))
                  >>= withPause

    w <- evWindow (False, protos, fst $ machine protos) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    img <- cam

    let v = feat img

    (click,pats,classify) <- getW w
    when click $ do
        let npats = (img, show (length pats)):pats
            nmach = fst $ machine npats
        putW w (False, npats, nmach)

    inWin w $ do
        drawImage img
        pointCoordinates (size img)
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)
        when (not $ null pats) $ do
            text2D 0.9 0.6 (classify img)

{-
    when (not $ null pats) $ inWin r $ do
        let x@((im,_),l) = minimumBy (compare `on` dist img) pats
            d = dist img x
        drawImage im
        pointCoordinates (mpSize 10)
        text2D 0.9 0.6 (l++": "++(show $ round d))
        when (d>10) $ do
            setColor 1 0 0
            lineWidth $= 10
            renderPrimitive Lines $ mapM_ vertex $
                [ Point 1 (-1), Point (-1) 1, Point 1 1, Point (-1) (-1) ]
-}

-----------------------------------------------------

dist (_,u) ((_,v),_) = n2 u v

n2 u v = sum $ map (^2) $ zipWith subtract u v

-----------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps,m) <- get st
    st $= (True,ps,m)

mouse _ st (Char 'f') Down _ _ = do
    (_,ps,_) <- get st
    sv <- openYUV4Mpeg (size $ fst $ head $ ps) (Just "catalog.avi") Nothing
    mapM_ (sv.fst) ps
    writeFile "catalog.labels" $ unlines $ [show n ++"\t"++l | (n,l) <- zip [1..length ps] (map snd ps)]

mouse def _ a b c d = def a b c d

------------------------------------------------------

getProtos sz feat = do
    opt <- getRawOption "--catalog"
    case opt of
        Nothing -> return []
        Just catalog -> do
            readCatalog (catalog++".avi") sz (catalog++".labels") Nothing feat
