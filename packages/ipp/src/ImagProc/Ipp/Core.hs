{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
-}
-----------------------------------------------------------------------------

module ImagProc.Ipp.Core
    ( Src, src, Dst, dst, checkIPP, roiSZ
    , module ImagProc.Ipp.Structs
    , module Image.Core
) where

import Image.Core
import ImagProc.Ipp.Structs

import Foreign.Ptr
import Control.Monad(when)
import ImagProc.Ipp.Wrappers
import Foreign.C.String(peekCString)
import Util.Misc(errMsg)

type family PtrOf (c :: *)

type instance PtrOf Word8  = Word8
type instance PtrOf Word16 = Word8
type instance PtrOf Word24 = Word8
type instance PtrOf Float  = Float

type Dst p t = Ptr (PtrOf p) -> Int -> IppiSize -> t

-- | Extracts from a destination Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
dst :: Image p -> (Dst p t) -> t
dst im f = if roiArea (roi im) > 0
             then f (castPtr $ starting im) (step im) (roiSZ (roi im))
             else error "empty destination ROI"


type Src p t = Ptr (PtrOf p) -> Int -> t

src :: Image p -> ROI -> Src p t -> t
src im r f = f (castPtr $ starting (setROI r im)) (step im)


roiSZ :: ROI -> IppiSize
roiSZ = adapt . roiSize
    where adapt (Size h w) = IppiSize (fromIntegral h) (fromIntegral w)


checkIPP :: String  -- ^ some identifier of the calling function
         -> IO CInt  -- ^ the ipp function to wrap
         -> IO ()
-- ^ Required wrapper to any ipp function, checking that it has been successful and touching the foreign pointers of the source images to prevent early deallocation.
checkIPP msg f = do
    err <- f
    when (err/=0) $ do
        ps <- ippGetStatusString err
        s <- peekCString ps
        if err > 0 || any (`elem` warnings) (words s)
          then
            errMsg $ "Warning in " ++ msg ++ ": " ++ s
          else
            error $ "in " ++ msg ++ ": " ++ s
    return ()
  where
    warnings = ["ippStsCoeffErr:"]

