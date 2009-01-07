module Util where

import EasyVision
import Numeric.LinearAlgebra
import Classifier
import Text.Printf

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
          ps = ejs "+" . sel (>0.5) $ candis
          ns = ejs "-" . sel (<0.2) $ candis
          ejs t = map (\r -> (overROI feat (r, imgproc), t))
          sel c = filter (c.overlap roi)

overROI feat (r, obj) = feat (modifyROI (const r) obj)

shErr d c = putStrLn $ (printf "error %.3f" $ 100 * errorRate d c) ++ " %"
shConf d c = putStrLn $ format " " (show.round) (confusion d c)
