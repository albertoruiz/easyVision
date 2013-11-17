{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------

module Image.Devel (
    RawImage,
    appG, appF, appC,
    getDataFileName
) where

import Image.Core
import Foreign.Ptr
import Foreign(Word8)

import Paths_hVision_base

----------------------------------------------------

type RawImage t = Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt -> t

appG :: RawImage t -> ImageGray -> t
appG f (G im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im

appF ::  RawImage t -> ImageFloat -> t
appF f (F im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im

appC :: RawImage t -> ImageRGB -> t
appC f (C im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im

