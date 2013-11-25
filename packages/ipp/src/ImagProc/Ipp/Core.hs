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

{-

-- | 'ROI'\'s area in pixels
validArea :: Image t -> Int
validArea = roiArea . roi

-- | Given an image, invalidROIs im computes a list of ROIs surrounding the valid ROI of im.
invalidROIs :: Image t -> [ROI]
invalidROIs img = [r | Just r <- thefour]
  where
    thefour = [ if r1>0   then Just $ ROI 0 (r1-1) 0 (w-1)     else Nothing
              , if r2<h-1 then Just $ ROI (r2+1) (h-1) 0 (w-1) else Nothing
              , if c1>0   then Just $ ROI r1 r2 0 (c1-1)       else Nothing
              , if c2<w-1 then Just $ ROI r1 r2 (c2+1) (w-1)   else Nothing
              ]
    ROI r1 r2 c1 c2 = roi img
    Size h w = size img

-}

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
    -- mapM_ (touchForeignPtr . fptr) ls -- really needed! --USE withImage
    return ()

warnings = ["ippStsCoeffErr:"]

