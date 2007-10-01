-- estimation of homographies between consecutive frames
-- examples:
-- ./brush penguin.dv

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point,Matrix)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import Data.List(minimumBy)
import Numeric.LinearAlgebra
import Control.Monad(when)
import GHC.Float(double2Float,isDoubleNaN)
import ImagProc.Ipp.Core
import Vision


data History = H { world  :: ImageFloat,
                   points :: [InterestPoint]
                 }

empty = do im <- image (Size 400 400)
           return $ H { world = im,
                        points = []
                      }


main = do
    args <- getArgs

    let sz = Size (480`div`2) (640`div`2)

    (cam, ctrl)  <- mplayer (args!!0) sz >>= withPause

    st <- empty

    state <- prepare st

    o <- createParameters state [("h",percent 30),
                                 ("smooth",intParam 3 0 10),
                                 ("alpha",realParam 0.9 0 1),
                                 ("see",realParam 1 0 10),
                                 ("umb",realParam 0.05 0 0.1),
                                 ("ranUmb", realParam 0.005 0 0.01),
                                 ("ranProb", realParam 0.99 0.5 1)]

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "track" sz Nothing (const (kbdcam ctrl)) state
    addWindow "warp" (Size 400 400) Nothing (const (kbdcam ctrl)) state

    10 `times` (const cam) $ undefined

    launch state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow st = do

    ph <- getParam param "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam param "smooth"

    --orig <- grab ocam
    orig <- cam
    im <- yuvToGray orig >>= scale8u32f 0 1

    ips <- getSaddlePoints smooth 7 h 500 20 10 im

    inWindow "camera" $ do
        drawImage im
        pointCoordinates (size im)
        setColor 0 0 1
        pointSize $= 3
        text2D 0.9 0 (show $ length ips)
        renderPrimitive Points (mapM_ vertex (map ipPosition ips))

    if length(points st) == 0 && (length ips > 5) -- first time
        then return st {points = ips}
        else if (length ips < 5)
                then return st
                else worker3 param inWindow (ips,im) st

---------------------------------------------------------------------

worker3 param inWindow (pl,im) st = do

    alpha  <- getParam param "alpha"
    --see    <- getParam param "see"
    umb    <- getParam param "umb"
    ranumb <- getParam param "ranUmb"
    prob   <- getParam param "ranProb"

    (pnew, pold, _) <- basicMatches (pl, points st) (distComb alpha) umb
                                            --(Just $ \c ->
                                            --inWindow "cost" (scale32f (-see) c >>= drawImage))



    if(length pnew < 10)
         then do return st
         else do let (h,inliers) = estimateHomographyRansac prob ranumb  (prep pnew) (prep pold)
                 inWindow "track" $ do
                     drawImage im
                     pointCoordinates (size im)
                     setColor 0 0 1
                     pointSize $= 1
                     text2D 0.9 0 (show $ length pl)
                     renderPrimitive Points (mapM_ vertex (map ipPosition pl))
                     setColor 1 0 1
                     pointSize $= 2
                     text2D 0.9 (-0.1) (show $ length pnew)
                     renderPrimitive Points (mapM_ vertex (map ipPosition pnew))
                     setColor 1 0 0
                     pointSize $= 4
                     text2D 0.9 (-0.2) (show $ length inliers)
                     renderPrimitive Points (mapM_ (vertex.fst) inliers)

                 warpOn (scaling 0.5 <> inv h) (world st) im
                 new <- warp (Size 400 400) (scaling 0.5 <> h <> scaling 2) (world st)

                 inWindow "warp" $ do
                     drawImage new

                 return st {points = pl,world = new}



text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s


---------------------------------------------------------

on f g = \x y -> f (g x) (g y)

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (dim u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]


