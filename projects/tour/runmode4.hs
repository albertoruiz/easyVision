-- non threaded GUI, returning result

import Vision.GUI
import ImagProc

main = do
    prepare
    r <- runNT camera (observe "image" rgb >>> arr (sum8u.grayscale))
    print r
    putStrLn "bye!"

