-- threaded GUI, returning result
-- (this mode drops display frames)

import Vision.GUI
import Image.Processing

main = do
    r <- runT camera (observe "image" rgb >>> arr (sumPixels.grayscale))
    print r
    putStrLn "bye!"

