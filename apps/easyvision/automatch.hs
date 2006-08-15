
module Main where

import Ipp
import Graphics.UI.GLUT hiding (Matrix, drawPixels, Size)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy,partition)
import GSL hiding (size)
import Vision

import qualified Ipp.Images as I

data MyState = ST { imgs :: [ImageFloat]
                  , corners, marked, orig::[InterestPoint]
                  , pts  :: [[[Double]]]
                  , basev ::Int
                  , smooth :: Int
                  }

diagl = diag . realVector

--------------------------------------------------------------
main = do
    args <- getArgs
    let sz = Size 288 384
    cam <- openCamera (args!!0) sz

    state <- prepare cam ST { imgs=[]
                            , corners=[], marked = [], orig = []
                            , pts=[]
                            , basev = 0
                            , smooth = 3
                            }

    addWindow "camera" sz Nothing marker state
    addWindow "selected" sz Nothing keyboard state

    launch state worker

-------------------------------------------------------------------
dist [a,b] [x,y] = sqrt $ (a-x)^2+(b-y)^2

pl (I.Point x y) = [x,y]
lpl = map (pl.ipPosition)

worker inWindow cam st = do

    camera <- grab cam
    camera32f <- scale8u32f 0 1 camera

    ips <- getSaddlePoints (smooth st) 7 0.05 200 20 10 camera32f

    let ps = map (closestBy distComb ips) (marked st)

    let ok p m = distSpatial p m < 30 && distFeat p m < 0.3

    let oks = zipWith ok ps (marked st)

    let okall = and oks

    when (length (orig st) == 5 && okall) $ do
        let (o:os) = lpl (orig st)
        let ([px,py]:pss) = lpl ps
        let h = estimateHomographyRaw os pss
        let r = inHomog $ h <> realVector [px,py,1]
        when (norm (r - realVector o)<3) $ do
            inWindow "selected" $ do
                warp (Size 288 384) h camera32f >>= drawImage
                pointCoordinates (Size 3 4)
                renderPrimitive LineLoop (mapM_ vertex (ht h (lpl ps)))


    inWindow "camera" $ do
        drawImage camera
        pointCoordinates (size camera)
        setColor 1 0 0
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex (map ipPosition ips))        
        setColor 0 0 1
        renderPrimitive Points (mapM_ vertex (map ipPosition (marked st)))
        pointSize $= 5
        setColor 0 0.5 0
        renderPrimitive Points (mapM_ vertex (map ipPosition ps))

        let vecs = concat $ map (\IP{ipPosition=I.Point x y,
                                     ipOrientation=a} -> [[x,y],[x+0.1*cos a,y+0.1*sin a]]) ps
        renderPrimitive Lines (mapM_ vertex vecs)


    let sel True p _ = p
        sel False _ n@IP{ipTime=t} = n {ipTime = t+1}

    let newmarked = zipWith3 sel oks ps (marked st)

    return st {corners = ips, marked = newmarked }


------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closestBy f [] p = p
closestBy f hp p = minimumBy (compareBy $ f p) hp


distFeat = (dist.:.ipDescriptor)
    where dist u v = norm (u-v)

distSpatial = (dist.:.ipRawPosition)
    where dist (I.Pixel a b) (I.Pixel x y) = sqrt $ fromIntegral ((a-x)^2+(b-y)^2)

f .:. g = h where h x y = f (g x) (g y)

distComb p q = distFeat p q + 0.1*distSpatial p q

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------
keyboard str (Char 'p') Down _ _ = do
    st <- readIORef str
    pause (camid st)
keyboard str (Char ' ') Down _ _ = do
    st <- readIORef str
    pause (camid st)
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

ip p = IP { ipDescriptor  = 0
          , ipRawPosition = p
          , ipPosition    = I.Point 0 0
          , ipOrientation = 0
          , ipTime        = 0
          }

marker str (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let u = ust (st)
    let newpoint = closestBy distSpatial (corners u) $ ip (I.Pixel (fromIntegral y) (fromIntegral x))
    print newpoint
    let m = newpoint : marked u
    if (length m /= 5)
        then writeIORef str st { ust = u { marked = m } }
        else writeIORef str st { ust = u { marked = m
                                         , orig = m
                                         }
                               }

marker st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = case marked (ust s) of
                                                        [] -> []
                                                        _:t -> t,
                                             orig = [] }}

marker st b s m p = keyboard st b s m p
