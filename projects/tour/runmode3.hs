-- non threaded GUI, discarding result

import Vision.GUI
import Image.Processing

main = do
    prepare
    runNT_ camera (observe "image" rgb >>> arr (sumPixels.grayscale))
    putStrLn "bye!"

