-- non threaded GUI, discarding result

import Vision.GUI
import ImagProc

main = do
    prepare
    runNT_ camera (observe "image" rgb >>> arr (sum8u.grayscale))
    putStrLn "bye!"

