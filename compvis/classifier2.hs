
import EasyVision
import ImagProc.Ipp.Core
import Control.Monad(when,(>=>))
import Graphics.UI.GLUT hiding (Point,Size)
import Data.List(minimumBy, transpose)
import Foreign


-- normalized lbp histogram
lbpN t im = unsafePerformIO $ do
    h <- lbp t im
    let ROI r1 r2 c1 c2 = theROI im
        sc = (256.0::Double) / fromIntegral ((r2-r1-1)*(c2-c1-1))
    return $ map ((*sc).fromIntegral) (tail h)

purelbp sz = lbpN 8 . resize sz . fromYUV

feat im = (im, purelbp (mpSize 5) im) -- (modifyROI (shrink (100,200)) im))

toYUV = unsafePerformIO . rgbToYUV

main = do
    sz <- findSize

    prepare

    (cam,ctrl) <- getCam 0 sz
                  >>= onlyRectangles (mpSize 10) (sqrt 2)
                  >>= virtualCamera (return . concat)
                  >>= virtualCamera (return . map toYUV)
                  >>= withPause

    protos <- getProtos feat

    w <- evWindow (False, protos) "video" sz Nothing  (mouse (kbdcam ctrl))

    r <- evWindow () "category" (mpSize 10)  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r)

-----------------------------------------------------------------

worker cam w r = do

    img@(orig,v) <- feat `fmap` cam

    (click,pats) <- getW w
    when click $ putW w (False, ((img, show (length pats))):pats)

    inWin w $ do
        drawImage orig -- (modifyROI (shrink (100,200)) orig)
        pointCoordinates (size orig)
        setColor 0 0 0
        renderAxes
        setColor 1 0 0
        renderSignal (map (*0.5) v)

    when (not $ null pats) $ inWin r $ do
        let x@((im,_),l) = minimumBy (compare `on` dist img) pats
            d = dist img x
        drawImage im
        pointCoordinates (mpSize 5)
        text2D 0.9 0.6 (l++": "++(show $ round d))
        when (d>10) $ do
            setColor 1 0 0
            lineWidth $= 10
            renderPrimitive Lines $ mapM_ vertex $
                [ Point 1 (-1), Point (-1) 1, Point 1 1, Point (-1) (-1) ]

-----------------------------------------------------

dist (_,u) ((_,v),_) = sum $ map (^2) $ zipWith subtract u v

-----------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse _ st (Char 'f') Down _ _ = do
    (_,ps) <- get st
    sv <- openYUV4Mpeg (size $ fst $ fst $ head $ ps) (Just "catalog.avi") Nothing
    mapM_ (sv.fst.fst) ps
    writeFile "catalog.labels" $ unlines $ [show n ++"\t"++l | (n,l) <- zip [1..length ps] (map snd ps)]

mouse def _ a b c d = def a b c d

------------------------------------------------------

getProtos feat = do
    opt <- getRawOption "--catalog"
    case opt of
        Nothing -> return []
        Just catalog -> do
            readCatalog (catalog++".avi") (mpSize 20) (catalog++".labels") Nothing feat

-- lectura de un vídeo etiquetado
readCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
readCatalog video sz classesfile mbn prepro = do
    cam <- mplayer (video++" -benchmark") sz
    rawclasses <- readFile classesfile
    let classfilelines = lines rawclasses
        effectivelines = case mbn of
            Nothing -> classfilelines
            Just n  -> take n classfilelines
    let n = length effectivelines
    let [frames, classes] = transpose $ map words effectivelines
    when (map read frames /= [1..n]) $ error ("inconsistent file "++ classesfile)
    imgs <- sequence (replicate n cam)
    return (zip (map prepro imgs) classes)
