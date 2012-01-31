import Vision.GUI
import ImagProc

main = do
    putStrLn "Working without GUI..."
    x <- runS camera  $    arr (sum8u.grayscale)
                      >>>  arrL (zip [1..] . take 1000)
    print x

