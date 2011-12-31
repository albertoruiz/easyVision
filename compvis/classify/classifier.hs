import EasyVision
import ImagProc.C.Segments
import Control.Monad(when,(>=>))
import Graphics.UI.GLUT hiding (Size,histogram)
import Classifier
import Numeric.LinearAlgebra
import Data.List(sortBy)
import Util.Stat
import Util.Misc(vec,Vec)
import Util.Options

pcaR r = mef (ReconstructionQuality r)

-- | distances (kernel?) to samples
distancesTo :: (a->b->Double) -> [b] -> a -> Vec
distancesTo f l x = vec (map (f x) l)

distancesToAll samp = distancesTo (\a b -> pnorm PNorm2 (a-b)) (map fst samp)

feat = andP [classi feat1, classi feat2]

feat' = const feat2

classi feat = normalizeAttr `ofP` pcaR 0.95 `ofP` distancesToAll `ofP` const feat
--                                outputOf (distance nearestNeighbour)

machine =  minDistance nearestNeighbour `onP` feat
--machine = detailed (minDistance nearestNeighbour `onP` feat)
--(distance mahalanobis `onP` (pcaR 0.9 `ofP` feat))

feat1 = vec . lbpN 8 . resize (mpSize 8) . grayscale

feat2 = vec . dw . histogramN [0..10] . hsvCode 80 85 175 . hsv
                                                       --135

dw (g:b:w:cs) = b:cs -- remove white

onlyCards sz = onlyRectangles segments sz (sqrt 2) rgb
               >=> virtualCamera (map channelsFromRGB . concat)

main = do
    sz <- findSize

    protos <- getProtos sz channels
    rects <- getFlag "--rectangles"
    let vc = if rects then onlyCards sz -- same size if we want to save more prototypes
                      else return . id

    prepare

    (cam,ctrl) <- getCam 0 sz ~> channels >>= vc >>= withPause

    w <- evWindow (False, protos, machine protos) "video" sz Nothing  (mouse (kbdcam ctrl))

    let prob = preprocess (feat protos) protos
        shprob = preprocess (mef (NewDimension 4) prob) prob

    --scatterWindow "scatter" (Size 400 400) shprob (0,1)

    launch (worker cam w)

-----------------------------------------------------------------

worker cam w = do

    img <- cam

    (click,pats,classify) <- getW w

    let v = toList $ feat pats img

    when click $ do
        let npats = (img, "?"):pats
            nmach = machine npats
        putW w (False, npats, nmach)

    inWin w $ do
        drawImage (rgb img)
        pointCoordinates (size (rgb img))
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)
        when (not $ null pats) $ do
            text2D 0.9 0.6 (show $ classify img)

-----------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps,m) <- getW st
    putW st (True,ps,m)

mouse _ st (Char 'f') Down _ _ = do
    (_,ps,_) <- getW st
    sv <- openYUV4Mpeg (size $ rgb $ fst $ head $ ps) "catalog.yuv" Nothing
    mapM_ (sv.yuv.fst) ps
    writeFile "catalog.labels" $ unlines $ [show n ++"\t"++l | (n,l) <- zip [1..length ps] (map snd ps)]

mouse def _ a b c d = def a b c d

------------------------------------------------------

getProtos sz feat = do
    opt <- getRawOption "--catalog"
    case opt of
        Nothing -> return []
        Just catalog -> getCatalog (catalog++".yuv") sz (catalog++".labels") Nothing feat

-- showB h l = unwords $ map fst $ filter ((<h).snd) l
