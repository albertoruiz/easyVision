{- save some frames with a ROI

$ ./roisel source --save=selected.yuv
  stop video with SPACE, mark roi with mouse right button, save desired frames/roi with S
  ESC to end.
$ ./roisel selected
  to browse the selected frames/rois with mouse wheel

$ ./roisel 'newvideo -benchmark' selectedtest selected1 selected2 etc.
  try to detect similar rois in a new video
  selectedtest is used for error estimation

-}

import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)
import System(getArgs)
import Data.Colour.SRGB
import qualified Data.Colour.Names as Col
import Numeric.LinearAlgebra
import Classifier
import Data.List(maximumBy)
import Text.Printf

setColor' c = setColor r g b where (r,g,b) = toSRGB c

genrois = roiGridStep 50 200 25 25
-- genrois = roiGridStep 100 100 25 25
commonproc = gray.channels
-- feat = vector .lbpN 8
feat = vector. map fromIntegral . lbp 8
-- machine =  distance gaussian `onP` mef (NewDimension 15)
-- machine = multiclass mse `onP` mef (ReconstructionQuality 0.95)
-- machine = distance ordinary
machine = multiclass mse

main = do
    args <- getArgs
    create <- getFlag "--save"
    if create
        then main1
        else if length args == 1
                then main2
                else main3

--------------------------------------------------------------

main1 = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare

    mbname <- getRawOption "--save"
    let name = case mbname of
                Nothing -> error "--save=filename.yuv is required"
                Just nm -> nm

    w <- evWindow False "Press S to save frame" sz Nothing  (mouse (kbdcam ctrl))

    save <- optionalSaver sz

    launch $ do
        orig <- cam
        rec <- getW w
        roi <- getROI w

        inWin w $ do
            drawImage orig
            drawROI roi
            when rec $ do
                save orig
                appendFile (name++".roi") (show roi++"\n")
                putW w False

mouse _ st (Char 's') Down _ _ = do
    st $~ not

mouse def _ a b c d = def a b c d

-----------------------------------------------------------------

main2 = do
    sz <- findSize
    file:_ <- getArgs
    prepare

    roisraw <- readFile (file++".yuv.roi")
    let rois = map read (lines roisraw) :: [ROI]
        nframes = length rois
    print nframes

    cam <- mplayer (file++".yuv -benchmark") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show (length imgs) ++ " cases"

    seeRois imgs rois
    mainLoop

seeRois imgs rois = evWindow 0 "Selected ROI" (mpSize 20) (Just disp) (mouse kbdQuit)
    where
    disp st = do
        k <- get st
        drawImage (imgs!!k)
        lineWidth $= 3
        setColor' Col.yellow
        drawROI (rois!!k)
        text2D 50 50 (show $ k+1)
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        k <- get st
        st $= min (k+1) (length imgs -1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        k <- get st
        st $= max (k-1) 0
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

------------------------------------------------------------------------------

main3 = do
    sz <- findSize
    video:test:files <- getArgs
    prepare

    rawexamples <- mapM (readSelectedRois sz) files
    let examples = concatMap (createExamples commonproc genrois feat) (concat rawexamples)
        (classifier, probs) = machine examples
        detector = head . probs

    putStrLn $ show (length examples) ++ " total examples"

    rawtest <- readSelectedRois sz test
    let test = concatMap (createExamples commonproc genrois feat) rawtest

    shConf test classifier
    shErr  test classifier

    -- print $ classifier $ constant 0 256
    -- evaluation required in "two places" to avoid
    -- relearning the classifier in each frame (!!??)

    (camtest,ctrl) <- mplayer video sz >>= detectStatic 0.01 5 >>= withPause
    w <- evWindow () "Plates detection" sz Nothing  (const (kbdcam ctrl))
    launch $ inWin w $ do
        img <- camtest
        drawImage img
        let candis = genrois (theROI img)
            imgproc = commonproc img
        lineWidth $= 1
        setColor' Col.gray
        mapM_ drawROI (filter ((=="+").classifier.feat.flip modifyROI imgproc. const) candis)
        let strength = detector . feat . flip modifyROI imgproc. const
        lineWidth $= 3
        setColor' Col.red
        drawROI $ maximumBy (compare `on` strength) candis

readSelectedRois sz file = do
    roisraw <- readFile (file++".yuv.roi")
    let rois = map read (lines roisraw) :: [ROI]
        nframes = length rois
    cam <- mplayer (file++".yuv -benchmark") sz
    imgs <- sequence (replicate nframes cam)
    putStrLn $ show nframes ++ " cases in " ++ file
    return (zip imgs rois)

createExamples commonproc candirois feat (img,roi) = ps ++ ns
    where candis = candirois (theROI img)
          imgproc = commonproc img
          prois = filter ((>0.5).overlap roi) candis
          nrois = filter ((<0.2).overlap roi) candis
          ps = map (\r -> (feat (modifyROI (const r) imgproc), "+")) prois
          ns = map (\r -> (feat (modifyROI (const r) imgproc), "-")) nrois

shErr d c = putStrLn $ (printf "error %.3f" $ 100 * errorRate d c) ++ " %"
shConf d c = putStrLn $ format " " (show.round) (confusion d c)