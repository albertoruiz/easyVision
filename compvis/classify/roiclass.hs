{- detection of rois based on examples labeled by roisel

$ ./roiclass 'newvideo -benchmark' selectedtest selected1 selected2 etc.

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
-- machine = multiclass mse `onP` mef (NewDimension 15)
-- machine = distance ordinary
machine = multiclass mse

------------------------------------------------------------------------------

main = do
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

    putStrLn $ show (length test) ++ " total test examples"
    


    shConf test classifier
    shErr  test classifier

--     error "hasta aq"
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
