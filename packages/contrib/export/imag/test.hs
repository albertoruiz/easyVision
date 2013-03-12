-- $ ./testh ../../../../data/images/calibration/cube1.png

import HTools

import Control.Applicative((<$>))
import System.Environment(getArgs)
import ImagProc

main = do
    img <- grayscale . channelsFromRGB <$> (loadRGB . head =<< getArgs)
    
    m <- funInit "data.txt"
    print (sum8u (fun m 0 img))
    print (sum8u (fun m 1 img))
    print (sum8u (fun m 2 img))
    print (sum8u (fun m 3 img))

