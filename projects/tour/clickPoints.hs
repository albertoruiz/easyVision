import Vision.GUI
import ImagProc
import System.Environment(getArgs)
import Data.Maybe(listToMaybe)
import Data.Traversable(traverse)

main = do
    mbfile <- listToMaybe `fmap` getArgs
    mbimg <- traverse loadRGB mbfile
    runIt $ do
        p <- clickPoints (sh mbimg)
        w <- browser "rectified" [] (const id)
        connectWith g p w

sh mbimg pts = Draw [ Draw mbimg
                    , color yellow . drawPointsLabeled $ pts]

g (k,_) ps = (k, [Draw ps, Draw (Closed ps)])

