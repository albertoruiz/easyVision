module Image.Devel(
    RawImage, appI,
    Wrap11, wrap11,
    fi, ti,
    (//), checkFFI,
    getDataFileName,
    module Image.Core,
    unsafePerformIO,
    CInt(..), Storable, Ptr
) where

import Paths_hVision_base
import Foreign.C.Types(CInt(..))
import Image.Core
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Storable(Storable(..))
import Control.Monad(when)
import Foreign.Ptr(Ptr)
import Util.Misc((//))

appI :: RawImage p t -> Image p -> t
appI f img = f (ptrAt img (Pixel 0 0)) (fi.step $ img) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . roi) img
    r1 (ROI r _ _ _) = r
    r2 (ROI _ r _ _) = r
    c1 (ROI _ _ c _) = c
    c2 (ROI _ _ _ c) = c

type RawImage p t = Ptr p -> CInt -> CInt -> CInt -> CInt -> CInt -> t

type Wrap11 p q = RawImage p (RawImage q (IO CInt))

wrap11 :: Storable b => String -> Wrap11 a b -> Image a -> Image b
wrap11 msg f x = unsafePerformIO $ do
    r <- newImage undefined (size x)
    withImage x $ withImage r $ checkFFI msg $
        appI f x `appI` r
    return r

checkFFI :: String -> IO CInt -> IO ()
checkFFI msg f = do
    err <- f
    when (err/=0)  (error $ "error in foreign function " ++ msg)

fi :: Int -> CInt
fi = fromIntegral
ti :: CInt -> Int
ti = fromIntegral

