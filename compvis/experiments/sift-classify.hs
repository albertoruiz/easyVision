-- image classification using interest points

import EasyVision
import Control.Monad(when)
import Graphics.UI.GLUT hiding (Point, Size)
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Vision
import ImagProc.GPU.SIFT
import EasyVision.MiniApps.SiftParams


main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    mbdb <- getRawOption "--load"

    prepare

    sift <- getSift
    os <- userSIFTParams
    matchGPU <- getMatchGPU

    o <- createParameters [("inlier_thres", intParam 3 0 20) ]

    sp <- os
    let proc (img,roi) = ((ch,roi), filter (inroi sz roi) (sift sp (gray ch)) )
            where ch = channels img

    db <- case mbdb of
        Nothing -> return []
        Just name -> map proc `fmap` readSelectedRois sz name

    w <- evWindow (False,db) "video" sz Nothing  (mouse (kbdcam ctrl))
    let d = height sz `div` 10
    evROI w $= ROI d (height sz-d) d (width sz-d)

    r <- evWindow () "category" sz  Nothing  (const (kbdcam ctrl))

    launch (worker cam w r sift os matchGPU o)

-----------------------------------------------------------------

worker cam w r sift os matcher o = do

    orig <- cam
    sp <- os
    roi <- getROI w

    inlier_thres <- getParam o "inlier_thres"

    let Size he wi = size (gray $ orig)
        v   = sift sp (gray orig)
        img = (orig, v)
        sz = size (gray orig)

    (click,pats) <- getW w
    when click $ putW w (False, ((orig,roi), filter (inroi sz roi) v) : pats)

    let dist x y = - (fromIntegral . length) (matches x y)
        matches (_,u) (_,v) = matcher 0.7 0.8 u v
        x = minimumBy (compare `on` dist img) pats
        ms = map (\[a,b]->(snd x !! a, snd img !! b)) (matches x img)
        d = length ms
        ok = not (null pats) && not (null v) && d > 5
        (h,inl) = compHomog inlier_thres ms
        roitrans = ht h . map p2l . pixelsToPoints sz . pixelsOfROI . snd . fst $ x
        li = length inl

    inWin w $ do
        drawImage (gray orig)
        setColor 0 0 0
        drawROI roi
        setColor 1 1 0
        pointCoordinates (size (gray orig))
        drawInterestPoints v
        setColor 1 0 0
        lineWidth $= 2
        when (ok && li > 4) $ renderPrimitive LineLoop $ mapM_ vertex roitrans


    inWin r $ when ok $ do
        drawImage $ gray $ fst $ fst x
        setColor 1 1 1
        drawROI $ snd $ fst x
        pointCoordinates (mpSize 5)
        
        text2D 0.9 0.6 (show (li, d - li))
        setColor 1 0 0
        pointSize $= fromIntegral inlier_thres*2+1
        renderPrimitive Points $ mapM_ (vertex.snd) inl        
        setColor 0.8 0.8 0.8
        drawInterestPoints (snd x)
        setColor 0 1 0        
        drawInterestPoints (map fst ms)



-----------------------------------------------------

inroi sz roi = inROI' sz roi . ipPosition

compHomog th matches = estimateHomographyRansac (fromIntegral (th::Int)/640) (f dest) (f orig) where
    (orig,dest) = unzip matches
    f = map (p2l.ipPosition)

p2l (Point x y) = [x,y]

pixelsOfROI (ROI r1 r2 c1 c2) =
    [ Pixel r1 c1
    , Pixel r1 c2
    , Pixel r2 c2
    , Pixel r2 c1 ]

-----------------------------------------------------

-- click to add a new object
mouse _ st (MouseButton LeftButton) Down _ _ = do
    (_,ps) <- get st
    st $= (True,ps)

mouse def _ a b c d = def a b c d

--------------------------------------------------------

readSelectedRois sz file = do
    roisraw <- readFile (file++".roi")
    let rois = map read (lines roisraw) :: [ROI]
        nframes = length rois
    cam <- mplayer (file++" -benchmark -loop 1") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show nframes ++ " cases in " ++ file
    return (zip imgs rois)
