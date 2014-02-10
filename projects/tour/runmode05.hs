import Image.Processing
import Image.Capture
import System.Environment

f = sumPixels . grayscale . channelsFromRGB

main = do
    folder:_ <- getArgs
    imgs <- readFolderIM folder
    print (map (f.fst) imgs)

