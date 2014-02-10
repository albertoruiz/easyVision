-- non threaded GUI, returning result

import Vision.GUI
import Image.Processing

main = do
    prepare
    r <- runNT camera (observe "image" rgb >>> arr (sumPixels.grayscale))
    print r
    putStrLn "bye!"

