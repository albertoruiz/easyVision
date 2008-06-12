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
            timing $ printf "%.1f\n" $ sum $ map hsum ims


hsum img = foldImage f 0 img
    where f !p !jump !i !j !s = s + float2Double (uval p jump i j)
{-# INLINE hsum #-}

count img = foldImage f (0::Double) img
    where f !p !jump !i !j !s = s+1
{-# INLINE count #-}

--------------------------------------------------------------------------

inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

--uval :: Ptr Float -> Int -> Int -> Int -> Float
uval !p !j !r !c = inlinePerformIO $ peekElemOff p (r*j+c)
{-# INLINE uval #-}

--foldImage :: (Ptr Float -> Int -> Int -> Int -> t -> t) -> t -> ImageFloat -> t
foldImage f x (i@(F img)) = inlinePerformIO $ withForeignPtr (fptr img) $ const $ return (go r1 c1 x)
    where ROI r1 r2 c1 c2 = vroi img
          p = castPtr (ptr img) :: Ptr Float
          jump = step img `quot` datasize img
          go !r !c !s = if r==r2 && c==c2
                         then (s + float2Double (uval p jump r c))
                         else if c==c2
                                 then go (r+1) c1   (s + float2Double (uval p jump r c))
                                 else go r    (c+1) (s + float2Double (uval p jump r c))
            where --here = f p jump r c s
                  !here = (s + float2Double (uval p jump r c))
                  {-# INLINE here #-}
