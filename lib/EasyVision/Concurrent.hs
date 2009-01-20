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

module EasyVision.Concurrent (
    asyncFun
)where

import Control.Concurrent
import Control.Monad
import Control.Parallel.Strategies
import ImagProc.Ipp.Core
import Features
import Numeric.LinearAlgebra(Vector, (@>))
import Storable(Storable)

instance NFData ImageYUV where
    rnf = rwhnf

instance NFData ImageFloat where
    rnf = rnf . (`fval` (Pixel 0 0))

instance NFData DetailedInterestPoint where
     rnf = rnf . ip
--     rnf = rnf . ipRawPosition

instance NFData InterestPoint where
     rnf = rnf . ipDescriptor

instance (NFData a, Storable a) => NFData (Vector a) where
     rnf = rnf . (@>0)

instance NFData Pixel where
     rnf (Pixel r c) = rnf r

concF put get d f n = do
    v <- n >>= newMVar . f
    forkIO $ forever $ do 
        when (d>0) (threadDelay d)
        x <- n
        let y = f x `using` rnf
        y `seq` put v y
    return (get v)

asyncFun d f n = concF swapMVar readMVar d f n

concFS f n = concF putMVar takeMVar 0 f n
