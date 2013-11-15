{-# LANGUAGE ForeignFunctionInterface,
             MagicHash,
             UnboxedTuples,
             BangPatterns,
             RecordWildCards,
             CPP #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Core
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPLu

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Experimental interface to Intel Integrated Performance Primitives for image processing.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Core
          ( -- * Image representation
            Img(..), ImageType(..)
            -- * Creation of images
          , img, imgAs, getData32f, setData32f, setData8u, setValue
            -- * Regions of interest
          , fullroi, invalidROIs, roiSZ, validArea, roiPtrs
            -- * Wrapper tools
          , src, dst, checkIPP, (//), starting
            -- * Image types
          , Image(..)
          , ImageRGB(C)
          , ImageGray(G)
          , ImageFloat(F)
          , ImageDouble(D)
          , ImageYUV (Y)
          , ImageYCbCr(Y422)
          -- * Image coordinates
          , val8u, fval
          -- * Reexported modules
          , module ImagProc.Ipp.Structs, CInt, CUChar, fi, ti
          , module Image.Base, module Image.ROI
) where

import Image.Core
import Image.Base
import Image.ROI
import ImagProc.Ipp.Structs

-- #if __GLASGOW_HASKELL__ >= 740
import Foreign.ForeignPtr.Unsafe
import Foreign.ForeignPtr(ForeignPtr,touchForeignPtr)
-- #else
-- import Foreign.ForeignPtr
-- #endif

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad(when)
import ImagProc.Ipp.Wrappers
import Foreign.C.String(peekCString)
import Foreign.C.Types
import GHC.Base
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Data.Binary
import Control.Applicative
import Util.Misc(assert)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString(ByteString)
import Control.DeepSeq


-- | Extracts from a destination Img the pointer to the starting position taken into account the given roi, and applies it to a ipp function.
dst :: Img -> ROI -> (Ptr a -> Int -> IppiSize -> t) -> t
dst im roi f = f (starting im roi) (step im) (roiSZ roi)

roiSZ = adapt . roiSize
    where adapt (Size h w) = IppiSize (fromIntegral h) (fromIntegral w)


checkIPP :: String  -- ^ some identifier of the calling function
         -> [Img]   -- ^ the source images required by the function
         -> IO Int  -- ^ the ipp function to wrap
         -> IO ()
-- ^ Required wrapper to any ipp function, checking that it has been successful and touching the foreign pointers of the source images to prevent early deallocation.
checkIPP msg ls f = do
    err <- f
    when (err/=0) $ do
        ps <- ippGetStatusString err
        s <- peekCString ps
        if err > 0 || any (`elem` warnings) (words s)
          then
            hPutStrLn stderr $ "Warning in " ++ msg ++ ": " ++ s
          else
            error $ "in " ++ msg ++ ": " ++ s
    mapM_ (touchForeignPtr . fptr) ls -- really needed!
    return ()

warnings = ["ippStsCoeffErr:"]


