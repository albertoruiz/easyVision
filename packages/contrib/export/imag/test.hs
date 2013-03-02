import HTools

import Control.Applicative((<$>))
import System.Environment(getArgs)
import ImagProc

main = do
    img <- grayscale . channelsFromRGB <$> (loadRGB . head =<< getArgs)
    
    print (sum8u img)
    print (sum8u (fun img))

