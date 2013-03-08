-- $ ./testh ../../../../data/images/calibration/cube1.png

import HTools

import Control.Applicative((<$>))
import System.Environment(getArgs)
import ImagProc

main = do
    img <- grayscale . channelsFromRGB <$> (loadRGB . head =<< getArgs)
    
    print (sum8u img)
    print (sum8u (fun img))
    
    m <- funparInit
    print (sum8u (funpar m img))

