import ImagProc
import System.Environment

f = sum8u . grayscale

main = do
    filename:_ <- getArgs
    img <- channelsFromRGB `fmap` loadRGB filename
    print (f img)
