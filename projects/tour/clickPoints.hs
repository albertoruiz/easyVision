import Vision.GUI
import ImagProc
import Data.Traversable(traverse)
import Util.Options(optionFromFile,getRawOption)

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    pts <- optionFromFile "--points" []
    
    runIt $ do
        p <- clickPoints (sh mbimg)
        putW p pts
        w <- browser "work with them" [] (const id)
        connectWith g p w

sh mbimg pts = Draw [ Draw mbimg
                    , color yellow . drawPointsLabeled $ pts]

g (k,_) ps = (k, [ pointSz 5 ps
                 , Draw (Closed ps)])

