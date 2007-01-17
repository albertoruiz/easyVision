-- This should work with any video source

import Ipp
import Graphics.UI.GLUT hiding (RGB,Size,Point,Matrix)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import Data.List(minimumBy)
import GSL hiding (size)
import qualified GSL
import Control.Monad(when)
import GHC.Float(double2Float,isDoubleNaN)
import Ipp.Core
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

    (cam, ctrl)  <- cameraRGB (args!!0) (Just sz)

    ocam <- openCamera (args!!0) sz
    --let ocam = undefined

    st <- empty

    state <- prepare ocam st

    o <- createParameters state [("h",percent 20),
                                 ("smooth",intParam 3 0 10),
                                 ("alpha",realParam 0.9 0 1),
                                 ("see",realParam 1 0 10),
                                 ("umb",realParam 0.05 0 0.1),
                                 ("ranumb", realParam 0.001 0 0.01)]

    addWindow "camera" sz Nothing (const (kbdcam ctrl)) state
    addWindow "cost"  (Size 300 300) Nothing undefined state
    addWindow "track" sz Nothing undefined state
    addWindow "warp" (Size 400 400) Nothing undefined state

    launch state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow ocam st = do

    ph <- getParam param "h" :: IO Int
    let h = fromIntegral ph / 100
    smooth <- getParam param "smooth"

    orig <- grab ocam
    --orig <- cam
    im <- rgbToGray orig >>= scale8u32f 0 1

    ips <- getSaddlePoints smooth 7 h 500 20 10 im

    inWindow "camera" $ do
        drawImage orig
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
    ranumb <- getParam param "ranumb"

    (pnew, pold, _) <- basicMatches (pl, points st) (distComb alpha) umb Nothing
                                            --(Just $ \c ->
                                            --inWindow "cost" (scale32f (-see) c >>= drawImage))



    if(length pnew < 10)
         then do print (length pnew)
                 return st
         else do let (h,inliers) = estimateHomographyRansac ranumb  (prep pnew) (prep pold)
                 print (length pnew, length inliers)
                 let guay = map fst inliers
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
                     text2D 0.9 (-0.2) (show $ length pnew)
                     renderPrimitive Points (mapM_ vertex guay)

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
    where dist u v = norm (u-v)/ fromIntegral (GSL.size u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

corresp umb h w simil = do
    (v, p@(Pixel r c)) <- maxIndx simil
    if (-v) > umb then return []
             else do set32f (-1000) simil ROI {r1=r,r2=r,c1=0,c2=w-1}
                     set32f (-1000) simil ROI {r1=0,r2=h-1,c1=c,c2=c}
                     sig <- corresp umb h w simil
                     return (p:sig)

basicMatches (pl,ql) dist umb dbgfun = do
    let n1 = length pl
    let n2 = length ql
    c <- image (Size n1 n2)
    let t = [[double2Float $ - dist p q | q <- ql] | p <- pl]
    setData32f c t
    case dbgfun of
        Nothing -> return ()
        Just f  -> f c
    corrs <- corresp umb n1 n2 c
    let pizq = map ((pl!!).row) corrs
    let pder = map ((ql!!).col) corrs
    return (pizq,pder,c)

---------------------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]


{-
    inWindow "track" $ do
        a <- scale32f 0.5 im1
        b <- scale32f 0.5 im2
        c <- a |+| b
        drawImage c
        pointCoordinates (size im2)
        setColor 0 0 0
        pointSize $= 3
        --renderPrimitive Points (mapM_ vertex (map ipPosition ips))
        setColor 1 0 0
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex (map ipPosition pizq))
        --renderPrimitive Points (mapM_ vertex (map ipPosition pder))
        setColor 0.5 0 0
        lineWidth $= 1
        let g a b = [a,b]
        renderPrimitive Lines $ mapM_ vertex $ concat (zipWith g (map ipPosition pizq) (map ipPosition pder))

        lineWidth $= 3
        setColor 0 0 1
        let h (p1,p2) = [p1,p2]
        renderPrimitive Lines $ mapM_ vertex $ concatMap h inliers

    inWindow "warp" $ do
        warp (Size 400 400) (scaling 0.5 <>h) im2 >>= drawImage

    return st
-}