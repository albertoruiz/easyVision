{-# OPTIONS -fbang-patterns #-}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

import EasyVision
--import Data.List(transpose,minimumBy,foldl1')
--import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
--import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import System.CPUTime
import GHC.Float(float2Double)
-- import Control.Monad(when)
-- import Control.Parallel.Strategies
import ImagProc.C.Simple(csum32f)

import GHC.Base
import GHC.IOBase

main = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare
--     o <- createParameters [("mode",intParam 0 0 2)]
    w <- evWindow () "image fold test" sz Nothing  (const (kbdcam ctrl))

    launchFreq 10 $ do
--         mode <- getParam o "mode" :: IO Int
        orig <- cam
        inWin w $ do
            let img = float . gray . channels $ orig
            drawImage img
            let ims = [k.*img|k<-[1..10]]
            putStrLn "------- "
            timing $ print $ sum $ map (fst.minmax) ims
            timing $ printf "%.1f\n" $ sum $ map sum32f ims
            timing $ printf "%.1f\n" $ sum $ map csum32f ims
            timing $ printf "%.1f\n" $ sum $ map (foldImage hsum 0) ims
--             timing $ printf "%.1d\n" $ sum $ map (foldImage hcount (0::Int)) ims


hsum !p !k !s = s + float2Double (uval p k)
{-# INLINE hsum #-}

hcount !p !k !s = s+1
{-# INLINE hcount #-}
--------------------------------------------------------------------------

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

uval !p !k = inlinePerformIO $ peekElemOff p k
{-# INLINE uval #-}

--foldImage :: (Ptr Float -> Int -> Int -> Int -> t -> t) -> t -> ImageFloat -> t
foldImage f x (i@(F img)) = inlinePerformIO $ withForeignPtr (fptr img) $ const $ return (go tot x)
    where jump = step img `div` datasize img
          ROI r1 r2 c1 c2 = vroi img
          p = advancePtr (castPtr (ptr img)) (r1*jump+c1) :: Ptr Float
          c = c2-c1+2
          tot = (r2 - r1) * jump + c2-c1

          go 0 s = f p (0::Int) s
          go !j !s = if j `rem` jump < c then go (j-1) (f p j s)
                                         else go (j-1) s

