-- work on several streams
-- $ ./concat '../../data/videos/rot4.avi -frames 25 -loop 1' '../../data/videos/rcube.avi -frames 25 -loop 1'

import Vision.GUI
import ImagProc
import ImagProc.Camera


main = do
    cams <- getCams
    prepare
    rs <- mapM (flip runNT (observe "image" rgb >>> arr (sum8u.grayscale))) (map return cams)
    mapM_ print rs
    putStrLn "bye!"

