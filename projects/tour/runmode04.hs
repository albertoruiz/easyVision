import Image.Processing
import System.Environment

f = sumPixels . grayscale

main = do
    filename:_ <- getArgs
    img <- channelsFromRGB `fmap` loadRGB filename
    print (f img)

