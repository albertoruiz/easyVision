-- ./multicam webcam1 logitech etc.

import EasyVision
import Text.Printf
import Data.List(tails,transpose)

main = do
    sz <- findSize
    n <- numCams

    run $   getMulticam sz n 
        ~~> history 2 
        ~>  difs
        >>= hist
        >>= observe "views" (\imgs -> blockImage [map grayscale imgs])
        >>= timeMonitor
    
----------------------------------------------------------------------------
    
hist cam = do 
   h <- signalMonitor "Sync" 50 100 (printf "%.0f gray levels") (0,640*480*255/10)
   return $ do
      (imgs,ds) <- cam       
      h ds
      return imgs

-----------------------------------------------------------------------------

history n = map (take n) . tails

difs imgs = (last imgs, map f (transpose imgs)) where
    f [x,y] = sum8u $ absDiff8u (grayscale x) (grayscale y)

------------------------------------------------------------------------------
