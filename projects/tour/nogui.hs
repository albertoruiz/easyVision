import Vision.GUI
import ImagProc

main = do
    putStrLn "Working without GUI..."
    x <- runS camera $ arrL (take 1000) >>> arr (sum8u.grayscale) >>> arrL (zip [1..])
    print x

