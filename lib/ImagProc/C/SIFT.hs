{-# LANGUAGE ForeignFunctionInterface, RecordWildCards #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.SIFT
Copyright   :  (c) Alberto Ruiz 2009
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Interface to the SiftGPU library by Changchang Wu, <http://www.cs.unc.edu/~ccwu>.

-}
-----------------------------------------------------------------------------


module ImagProc.C.SIFT(
    getSift, 
    SIFTParam(..), defaultSIFTParam
) where

import ImagProc.Ipp.Core
import ImagProc.Generic(clone)
import ImagProc.Ipp.AdHoc(set8u)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Numeric.LinearAlgebra
import Data.Packed.Development
import Graphics.UI.GLUT hiding (Point)

-----------------------------------------------------------------

data SIFTParam = SIFTParam
    { oct1   :: Int
    , thres  :: Double
    , nmax   :: Int
    }

defaultSIFTParam = SIFTParam
    { oct1  = 0
    , thres = 0.00666
    , nmax  = 1000
    }

getSift :: IO (SIFTParam -> ImageGray -> [InterestPoint])

inContext cxt f = do
    saved <- get currentWindow
    currentWindow $= Just cxt
    r <- f
    currentWindow $= saved
    return r

getSift = do
    w <- createWindow "aux SIFTGPU"
    windowStatus $= Hidden
    return (\ p x -> unsafePerformIO $ inContext w (sift p x))


sift pars (G im) = do
    r <- createVector ((128+4)*nmax pars)
    ptot <- malloc
    (argc,argv) <- parsToStrings pars
    app1 (c_SIFT_GPU (fi argc) argv
                     (castPtr (ptr im)) (step im)
                     (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                     ptot)
         vec r "c_SIFT_GPU"
    touchForeignPtr (fptr im)
    tot <- peek ptot
    free ptot
    mapM_ free =<< peekArray argc argv
    free argv
    if tot > 0
        then return $ map toIP
                    $ toRows
                    $ reshape (128+4) 
                    $ (subVector 0 (fromIntegral ((128+4)*tot)) r)
        else return []

toIP v = IP { ipPosition    = Point (v@>0) (v@>1)
            , ipScale       = v@>2
            , ipOrientation = v@>3
            , ipDescriptor  = subVector 4 128 v
            }

foreign import ccall "Simple/simple.h c_siftGPU"
    c_SIFT_GPU :: CInt -> Ptr (CString)
               -> Ptr CUChar -> Int -> Int -> Int -> Int -> Int
               -> Ptr CInt -> CInt -> Ptr Double -> IO CInt

parsToStrings SIFTParam {..} = do
    let sp = [ "-fo", show oct1
             , "-t" , show thres
             , "-cuda"
             , "-f", "4.0"
             , "-w", "2.0"
             , "-dw","3.0"
             , "-e", "10.0"
             , "-v", "0"
             ,"-sign"
             ]
    argv <- newArray =<< mapM newCString sp
    return (length sp,argv)