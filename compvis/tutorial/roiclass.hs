-- detection of rois based on examples labeled by roisel
-- $ ./roiclass 'newvideo -benchmark' test train1 train2 etc.
import EasyVision
import Graphics.UI.GLUT hiding (histogram)
import Control.Monad(when)
import System.Environment(getArgs)
import Numeric.LinearAlgebra
import Classifier
import Data.List(maximumBy)
import Util.Misc(vec)
import Util.Probability(evidence)
import Util

genrois = roiGridStep 50 200 25 25
commonproc = highPass8u Mask5x5 . median Mask5x5 . grayscale . channels
feat = vec . map fromIntegral . histogram [0,8 .. 256]
machine = multiclass mse

main = do
    sz <- findSize
    video:test:files <- getArgs
    prepare
    rawexamples <- mapM (readSelectedRois sz) files
    let examples = concatMap (createExamples commonproc genrois feat) (concat rawexamples)
        classifier = machine examples
    putStrLn $ show (length examples) ++ " total examples"

    rawtest <- readSelectedRois sz test
    let test = concatMap (createExamples commonproc genrois feat) rawtest
    putStrLn $ show (length test) ++ " total test examples"
--    shQuality test classifier
    shConf test (Just . mode . classifier)
    shErr  test (Just . mode . classifier)

    (camtest,ctrl) <- mplayer video sz >>= detectStatic 0.01 5 5 (grayscale.channels) id >>= withPause
    w <- evWindow () "Plates detection" sz Nothing  (const (kbdcam ctrl))
    launch $ inWin w $ do
        img <- camtest
        drawImage img
        let imgproc = commonproc img
            candis = map (\r -> (r,imgproc)) (genrois (theROI img))
        lineWidth $= 1
        setColor' gray
        let pos = filter ((=="+"). mode . classifier. overROI feat) candis
        mapM_ (drawROI.fst) pos
        when (not.null $ pos) $ do
            lineWidth $= 3
            setColor' red
            let best = maximumBy (compare `on` (evidence "+" . classifier) . overROI feat) pos
            drawROI (fst best)

