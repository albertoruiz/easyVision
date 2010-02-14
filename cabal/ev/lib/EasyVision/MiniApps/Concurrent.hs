-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.Concurrent
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Concurrency utilities.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Concurrent (
    asyncFun,
    syncFun,
    mkWatch,
    (|***|),
    pipeline
) where

import Control.Concurrent
import Control.Monad
import Control.Parallel.Strategies
import ImagProc.Ipp.Core
import Features
--import Numeric.LinearAlgebra(Vector, (@>))
--import Storable(Storable)
import Data.IORef
import Debug.Trace

instance NFData ImageYUV where
    rnf = rwhnf

instance NFData ImageFloat where
    rnf = rnf . (`fval` (Pixel 0 0))

instance NFData ImageGray where
    rnf = rnf . fi . (`val8u` (Pixel 0 0)) where fi x = fromIntegral x :: Int


instance NFData DetailedInterestPoint where
    rnf = rwhnf . ip
--    rnf = rnf . ipRawPosition

instance NFData InterestPoint where
     rnf = rwhnf . ipDescriptor

--instance (NFData a, Storable a) => NFData (Vector a) where
--     rnf = rnf . (@>0)

instance NFData Pixel where
     rnf (Pixel r c) = rnf r

instance NFData Point where
     rnf (Point x y) = rnf x


asyncFun d f n = do
    v <- n >>= newMVar . f
    forkIO $ forever $ do 
        when (d>0) (threadDelay d)
        x <- n
        let y = f x `using` rnf
        y `seq` swapMVar v y
    return (readMVar v)


syncFun f k n = do
    x <- fmap f n
    o:vs <- replicateM (k+1) (newMVar x)
    forkIO $ forever $ do
        x <- n
        let y = f x `using` rnf
            r:rs = vs
            put z = swapMVar o z >> mapM_ (forkIO . flip putMVar z) rs >> putMVar r z
        y `seq` put y
    return (fmap takeMVar vs, readMVar o)

mkWatch :: IO (IO a) -> IO (IO a, IO a)
mkWatch gencam = do
    cam <- gencam
    v <- newEmptyMVar
    fstTime <- newIORef True
    let newCam = do
            x <- cam
            ft <- readIORef fstTime
            if ft then putMVar v x 
                  else swapMVar v x >> return ()
            writeIORef fstTime False
            return x
        watch = readMVar v
    return (newCam,watch)

-----------------------------------------------------

f |***| g = h where
    h (x,y) = (f x, g y) `using` parPair rnf rnf

-----------------------------------------------------

pipeline f (a:b:rest) = fa : pipeline f (b':rest) where
    (fa, b') = (f a, b) `using` parPair rnf rnf

