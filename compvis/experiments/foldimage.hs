{-# OPTIONS -fbang-patterns #-}

import EasyVision
--import Data.List(transpose,minimumBy,foldl1')
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import System.CPUTime
import GHC.Float(float2Double)
-- import Control.Monad(when)
-- import Control.Parallel.Strategies
import ImagProc.C.Simple(csum32f)
import Numeric.LinearAlgebra hiding ((.*))
import ImagProc.ImageFold
import Features.Descriptors
import Vision(unitary,rot3)


main = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare
    o <- createParameters [("sc",realParam 2 0 5),
                           ("rot",realParam 0 (-180) 180)]
    w <- evWindow () "image fold test" sz Nothing  (const (kbdcam ctrl))

    launchFreq 25 $ do
        sc <- getParam o "sc"
        rot <- getParam o "rot"
        orig <- cam
        inWin w $ do
            let img = warp 0 (size orig) (rot3 (rot*pi/180)) $ float . gray . channels $ orig
            drawImage img
            let ims = [modifyROI (shrink (2,2)) (k.*img)|k<-[1..10]]
            let roi = ROI 201 300 201 300
                g = gradients $ (sc .*) $ gaussS 1 $ modifyROI (const roi) img
            --drawROI (theROI (gm g))
            drawImage (gy g)
            putStrLn "------- "
            timing $ print $ sum $ map (fst.minmax) ims
            timing $ print $ (fst.minmax) (gm g)
            timing $ printf "%.1f\n" $ sum $ map sum32f ims
            timing $ printf "%.1f\n" $ sum $ map csum32f ims
            timing $ printf "%.1f\n" $ sum $ map (foldImage hsum 0) ims
            timing $ printf "pixNum = %.1d\n" $ sum $ map (foldImage hcount (0::Int)) ims
            let hd = histodir (gm g) (gx g) (gy g)
--             timing $ print hd
            setColor 1 0 0
            let xroi = theROI (gm g)
            drawVector (5+c1 xroi) (r2 xroi) (1000*hd)
            let sd = usurfRaw 3 (gx g,gy g) roi
            drawVector (200+c1 xroi) (r2 xroi) (100*sd)
            text2D 20 20 $ printf "%.0f" $ head $ map (*(180/pi)) $ angles hd


hsum !p !k !s = s + float2Double (uval p k)
{-# INLINE hsum #-}

hcount !p !k !s = s+1
{-# INLINE hcount #-}

sumv v = v <.> constant 1 (dim v)
