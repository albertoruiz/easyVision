-- concatenate sources
-- $ ./concat '../../data/videos/rot4.avi -frames 25 -loop 1' '../../data/videos/rcube.avi -frames 25 -loop 1'

import Vision.GUI
import ImagProc
import ImagProc.Camera
import Util.LazyIO


concatCams ::  [IO (Maybe Channels)] -> Generator Channels
concatCams = (mkGenerator =<<) . fmap concat . sequence . map lazyList


main = do
    cams <- getCams
    print (length cams)
    prepare
    r <- runNT (concatCams cams) (observe "image" rgb >>> arr (sum8u.grayscale))
    print r
    putStrLn "bye!"

