-- threaded GUI, returning result
-- (this mode drops display frames)

import Vision.GUI
import ImagProc

main = do
    r <- runT camera (observe "image" rgb >>> arr (sum8u.grayscale))
    print r
    putStrLn "bye!"

