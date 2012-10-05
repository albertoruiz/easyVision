import ImagProc
import ImagProc.Camera
import System.Environment

f = sum8u . grayscale

main = do
    folder:_ <- getArgs
    imgs <- readFolderIM folder
    print (map (f.fst) imgs)
