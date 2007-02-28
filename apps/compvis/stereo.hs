
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import GHC.Float(isDoubleNaN)
import ImagProc.Ipp.Core
import Data.List(minimumBy)
import GSL hiding (size)
import qualified GSL
import Control.Monad(when)
import Vision

--import Ipp.Core

import qualified Data.Map as Map

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = if Map.member "--size" opts
                 then mpSize $ read $ Map.findWithDefault "20" "--size" opts
                 else Size (read $ Map.findWithDefault "480" "--rows" opts)
                           (read $ Map.findWithDefault "640" "--cols" opts)


    (cam1,ctrl1) <- mplayer (args!!0) sz >>= withPause
    (cam2,ctrl2) <- mplayer (args!!1) sz >>= withPause

    state <- prepare ()

    addWindow "left" sz Nothing  (const (kbdcam ctrl1)) state
    addWindow "right" sz Nothing (const (kbdcam ctrl2)) state
    addWindow "stereo" sz Nothing   (const (kbdcam ctrl1)) state

    opts <- createParameters state [("h",percent 20),
                                    ("smooth",intParam 3 0 10),
                                    ("umb",realParam 0.05 0 0.1),
                                    ("ranumb", realParam 0.001 0 0.01)]

    --sequence (replicate 30 cam1)
    --sequence (replicate 30 cam2)


    launch state (worker cam1 cam2 opts)

-----------------------------------------------------------------


worker cam1 cam2 opts inWindow _ = do

    ph <- getParam opts "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam opts "smooth"
    umb <- getParam opts "umb"

    im1 <- cam1 >>= yuvToGray >>= scale8u32f 0 1 
    im2 <- cam2 >>= yuvToGray >>= scale8u32f 0 1


    ips1 <- getSaddlePoints smooth 7 h 500 20 10 im1
    ips2 <- getSaddlePoints smooth 7 h 500 20 10 im2

    (good1, good2 , _) <- basicMatches (ips1, ips2) (distFeat) umb

    inWindow "left" $ do
        drawImage im1
        pointCoordinates (size im1)
        setColor 0 0 1
        pointSize $= 2
        renderPrimitive Points (mapM_ vertex (map ipPosition ips1))
        setColor 1 0 0
        pointSize $= 3
        --text2D 0.9 0 (show $ length ips1)
        renderPrimitive Points (mapM_ vertex (map ipPosition good1))

    inWindow "right" $ do
        drawImage im2
        pointCoordinates (size im2)
        setColor 0 0 1
        pointSize $= 2
        renderPrimitive Points (mapM_ vertex (map ipPosition ips1))
        setColor 1 0 0
        pointSize $= 3
        --text2D 0.9 0 (show $ length ips2)
        renderPrimitive Points (mapM_ vertex (map ipPosition good2))

    when (length good1 > 10 && length good1 <50) $ inWindow "stereo" $ do
        ranumb <- getParam opts "ranumb"
        let (h,inliers) = estimateHomographyRansac ranumb (prep good1) (prep good2)
        --print h
        warp (size im2) h im2 >>= drawImage


    return ()

on f g = \x y -> f (g x) (g y)

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (GSL.size u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]
