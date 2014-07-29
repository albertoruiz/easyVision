{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP.Core
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
-}
-----------------------------------------------------------------------------

module Image.Processing.IPP.Core
    ( Src, src, Dst, dst, checkIPP, roiSZ, Word8, Word16
    , module Image.Processing.IPP.Structs
    , module Image.Devel
) where

import Image.Devel
import Image.Processing.IPP.Structs
import Image.Processing.IPP.Wrappers
import Foreign(Word8,Word16)
import Foreign.Ptr
import Foreign.C.String(peekCString)
import Util.Debug(errMsg)
import Control.Monad(when)

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

