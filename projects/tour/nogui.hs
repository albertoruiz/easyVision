import Vision.GUI
import Image.Processing

main = do
    putStrLn "Working without GUI..."
    x <- runS camera  $    arr (sumPixels.grayscale)
                      >>>  arrL (zip [1..] . take 1000)
    print x

