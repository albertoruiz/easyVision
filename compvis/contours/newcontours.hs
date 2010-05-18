-- affine alignment of contours
-- ./contours tv://

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,normalize)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Vision
import Data.List
import Control.Monad(guard)
import Data.Array
import Data.Fixed(mod')
import Text.Printf(printf)
import ImagProc.Ipp.Core
import Debug.Trace
import Util.Misc(pairsWith)
import GHC.Float(double2Float)

----------------------------------------------------------

debug' f x = trace (show $ f x) x

assert msg f x = if f x then x else error (msg ++ " failed!!!")

-----------------------------------------------------------

normalize g = map autoRot $ concat (return $ head $ pairsWith (autoStart g) (candis g))

autoRot h = rot (-phase (h 1)) h

-- autoStart g = hs where
--     [i',j'',k] = debug' id $ take 3 . sortBy (compare `on` (negate.magnitude.g)) $  [-10 .. -1] ++ [1..10]
--     quality = magnitude (g j'') / magnitude (g k)
--     j' | quality > 1.2 = j''
--        | abs j'' < abs k = j''
--        | abs j'' == abs k && j'' > 0 = j''
--        | otherwise = debug' (const "cambio") k
--     [i,j] = if i'>j' then [i',j'] else [j',i']

candis g = if magnitude (g 2) / magnitude (g 1) > 0.09
    then [2,1]
    else [1,-3]

candis' g = filter ((>0.5*x).magnitude.g) ws where
    ws = reverse $  [-10 .. -1] ++ [1..10]
    xs = sortBy (compare `on` (negate.magnitude.g)) $ ws
    x = magnitude (g $ xs!!1)


autoStart g i j = [head (hs)] where -- [hs!!(s `mod` (length hs))] where -- or hs
    -- i>j
    m = assert "i>j" (>0) $ fromIntegral (i-j)

    delta = (phase (g j) - phase (g i)) `mod'` (2*pi)
    n1 = 0
    n2 = fromIntegral $ floor $ m-delta/(2*pi)
    ns = [n1 .. n2]
    ds = [(delta+2*pi*n)/m | n <- ns]
    hs = map (flip shiftStart g) ds
--    h = minimumBy (compare `on` df) hs
--    df h = (phase thing - phase (h i)) `mod'` (2*pi)
--        where thing = sum $ map h $ [-10..10] \\ [0,i,j]

-----------------------------------------------------------


htp h (Closed c) = Closed . map l2p . ht h . map p2l $ c
p2l (Point x y) = [x,y]
l2p [x,y] = Point x y
p2c (Point x y) = x :+ y
c2p (x:+y) = Point x y

fst3 (a,_,_) = a

times n f = (!!n) . iterate f

------------------------------------------------------------

pru = [Point 0 0, Point 0 (0.5), Point (-0.5) (0.75), Point 0 1, Point (0.5) (0.5), Point 1 0, Point (0.5) 0]

pru' = map (\x-> Point x 0) [10 * cos(2 * pi * 3 * x - pi/4)| x <- [0,delta .. 2-delta]] where delta = 2/200

fpl = fourierPL (Closed pru)

main' = do
    print pru
    print (fft (fromList $ map p2c pru) / scalar (fromIntegral (length pru)))
    putStrLn "------------------------"
    print (fpl 1)
    putStrLn "------------------------"
    mapM_ print (map fpl [-10 .. 10])


main = do
    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz >>= withPause

    state <- prepare' ()

    o <- createParameters [
        ("umbral2",intParam 128 1 255),
        ("area",percent 1),
        ("fracpix",realParam (1.5) 0 10),
        ("comps",intParam 8 1 20),
        ("white",intParam 0 0 1),
        ("eps",realParam 0.03 0 0.2),
        ("smooth2",intParam 1 0 10),
        ("rotation",realParam 0 (-pi) pi),
        ("showFou",intParam 0 0 1),
        ("selected",intParam 0 0 10)
     ]

    addWindow "contours" sz Nothing (const (kbdcam ctrl)) state

    launch' state (worker cam o)

-----------------------------------------------------------------

worker cam param inWindow _ = do


    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area"
    fracpix <- getParam param "fracpix"
    comps <- getParam param "comps"
    white <- getParam param "white"
    showFou <- getParam param "showFou" :: IO Int
--    eps <- getParam param "eps"
--    rotation <- getParam param "rotation"
    selected <- getParam param "selected" :: IO Int

    orig <- cam >>= return . yuvToGray
    let im = (smooth2 `times` median Mask3x3) orig

    let (Size h w) = size im
        pixarea = h*w*area`div`1000
        rawconts = contours 100 pixarea th2 (toEnum white) im
        proc = Closed . pixelsToPoints (size orig).douglasPeuckerClosed fracpix.fst3
        cs = map proc $ rawconts
        wcs = map whitenContour cs
        fwcs = map fourierPL wcs
        pos = map (c2p .($0)) fwcs
        r21 = map g fwcs where g f = printf "%.2f" (magnitude (f 2) / magnitude (f 1))
        nfs = concatMap normalize fwcs

    inWindow "contours" $ do
             drawImage orig
             pointCoordinates (size im)
             lineWidth $= 1
             setColor 1 1 0
             mapM_ (shcont. invFou 50 comps) nfs
             lineWidth $= 2
             setColor 0 0 1
             if (showFou==0) 
                then mapM_ (shcont) cs
                else mapM_ (shcont. invFou 50 comps . fourierPL) cs
             setColor 1 0 1
             sequence_ $ zipWith textP pos r21
    return ()


-------------------------------------------

textP (Point x y) s = text2D (double2Float x) (double2Float y) s


shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 5
    renderPrimitive Points (vertex (head c))


shiftStart r f = \w -> cis (fromIntegral w*r) * f w

invFou n w fou = Closed r where
    f = fromList $ map fou [0..w] ++ replicate (n- 2*w - 1) 0 ++ map (fou) [-w,-w+1.. (-1)]
    r = map c2p $ toList $ ifft (fromIntegral n *f)


rot r f = g where g 0 = f 0
                  g w = cis (r) * f w

