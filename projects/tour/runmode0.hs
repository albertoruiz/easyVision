-- no gui

import Vision.GUI
import Image.Processing

main = do
    putStrLn "Working without GUI..."
    x <- runS camera  $    arr (sumPixels . grayscale)
                      >>>  arrL (zip [1..] . take 10)
    print x
    print (length x)
    putStrLn "Bye"

{--------------------------------------------------------------------

tests:

work on a long video, take the initial sublist:

$ ./runmode0
Working without GUI...
[(1,3.1243257e7),(2,4.0164279e7),(3,3.9031153e7),(4,3.9024777e7),(5,3.9032666e7),
(6,3.9097742e7),(7,3.9104864e7),(8,3.9101045e7),(9,3.9166347e7),(10,3.9168896e7)]
10
Bye

work on a short video:

 ./runmode0 '../../data/videos/rot4.avi -frames 5 -loop 1'
Working without GUI...
[(1,3.1425134e7),(2,3.1350479e7),(3,3.1274583e7),(4,3.1188253e7),(5,3.1112087e7)]
5
Bye


----------------------------------------------------------------------}

