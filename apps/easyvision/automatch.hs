
module Main where

import Ipp hiding (shift)
import Graphics.UI.GLUT hiding (Matrix, drawPixels)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy,partition)
import GSL
import Vision

import Saddle

type Point = [Double]  -- provisional
type Pixel = [Int]

data MyState = ST { imgs :: [Img]
                  , corners, marked, orig::[InterestPoint]
                  , pts  :: [[Point]]
                  , basev ::Int
                  , smooth :: Double
                  }

diagl = diag . realVector

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam ST { imgs=[]
                            , corners=[], marked = [], orig = []
                            , pts=[]
                            , basev = 0
                            , smooth = 3
                            }

    addWindow "camera" (w,h) Nothing marker state
    addWindow "selected" (w,h) Nothing keyboard state

    launch state worker

-------------------------------------------------------------------
dist [a,b] [x,y] = sqrt $ (a-x)^2+(b-y)^2

worker inWindow camera st = do

    ips <- getSaddlePoints (smooth st) 7 0.05 200 20 10 camera

    let ps = map (closestBy distComb ips) (marked st)

    let ok p m = distSpatial p m < 30 && distFeat p m < 0.3

    let oks = zipWith ok ps (marked st)

    let okall = and oks

    when (length (orig st) == 5 && okall) $ do
        let (o:os) = map ipPosition (orig st)
        let ([px,py]:pss) = map ipPosition ps
        let h = estimateHomographyRaw os pss
        let r = inHomog $ h <> realVector [px,py,1]
        when (norm (r - realVector o)<3) $ do
            inWindow "selected" $ do
                scale8u32f 0 1 camera >>= warp (288,384) h >>= drawImage
                pointCoordinates (4,3)
                renderPrimitive LineLoop (vertices (ht h (map ipPosition ps)))


    inWindow "camera" $ do
        drawImage camera
        pixelCoordinates (384,288)
        mycolor 1 0 0
        pointSize $= 3
        renderPrimitive Points (vertices (map ipRawPosition ips))
        pixelCoordinates (384,288)
        mycolor 0 0 1
        renderPrimitive Points (vertices (map ipRawPosition $ marked st))
        pointSize $= 5
        mycolor 0 0.5 0
        renderPrimitive Points (vertices (map ipRawPosition ps))

        pointCoordinates (4,3)
        let vecs = concat $ map (\IP{ipPosition=[x,y],
                                     ipOrientation=a} -> [[x,y],[x+0.1*cos a,y+0.1*sin a]]) ps
        renderPrimitive Lines (vertices vecs)


    let sel True p _ = p
        sel False _ n@IP{ipTime=t} = n {ipTime = t+1}

    let newmarked = zipWith3 sel oks ps (marked st)

    return st {corners = ips, marked = newmarked }


------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closestBy f [] p = p
closestBy f hp p = minimumBy (compareBy $ f p) hp


distFeat = (dist.:.(realVector.ipDescriptor))
    where dist u v = norm (u-v)

distSpatial = (dist.:.ipRawPosition)
    where dist [a,b] [x,y] = sqrt $ (a-x)^2+(b-y)^2

f .:. g = h where h x y = f (g x) (g y)

distComb p q = distFeat p q + 0.1*distSpatial p q

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard st (Char ' ') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {basev = basev (ust s) + 1}}
keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {basev = max (basev (ust s) - 1) 0}}

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}

keyboard _ _ _ _ _ = return ()

------------------------------------------------------------------

marker str (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let u = ust (st)
    let newpoint = closestBy distSpatial (corners u) $ ip (map fromIntegral [x,y])
    print newpoint
    let m = newpoint : marked u
    if (length m /= 5)
        then writeIORef str st { ust = u { marked = m } }
        else writeIORef str st { ust = u { marked = m
                                         , orig = m
                                         }
                               }
{-
           do
            --let hp = reverse $ pixelToPoint (camera st) (map fst m)
            im  <- scale8u32f 0 1 (camera st)
            let v = u { marked = []
                       , imgs = imgs u ++ [im]
              --         , pts = pts u ++ [hp]
                       }
            writeIORef str st { ust = v }
-}


marker st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = case marked (ust s) of
                                                        [] -> []
                                                        _:t -> t,
                                             orig = [] }}

marker st b s m p = keyboard st b s m p

