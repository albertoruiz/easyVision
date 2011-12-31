{-# LANGUAGE ForeignFunctionInterface, RecordWildCards, TemplateHaskell #-}

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


module ImagProc.GPU.SIFT(
    getSift,
    SIFTParams(..), defSIFTParams, winSIFTParams, argSIFTParams,
    getMatchGPU
) where

import ImagProc.Ipp.Core
import Foreign
import Foreign.C.String
import Numeric.LinearAlgebra hiding (step)
import Data.Packed.Development
import Graphics.UI.GLUT hiding (Point)
import Control.Monad(when)
import EasyVision.GUI.Parameters

-----------------------------------------------------------------

$(autoParam "SIFTParams" ""
        [ ("oct1" , "Int",    intParam    0        0 3)
        , ("thres", "Double", realParam   0.00666  0 0.01)
        , ("nmax",  "Int",    intParam    1000     0 2000)
        ]
 )


getSift :: IO (SIFTParams -> ImageGray -> [InterestPoint])

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
    let ok = vroi im == fullroi im
           && step im == width (isize im)
    when (not ok) $ error "FIXME: Sift GPU on image with ROI or step"
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

foreign import ccall "c_siftGPU"
    c_SIFT_GPU :: CInt -> Ptr (CString)
               -> Ptr CUChar -> Int -> Int -> Int -> Int -> Int
               -> Ptr CInt -> CInt -> Ptr Double -> IO CInt

parsToStrings SIFTParams {oct1 = o, thres = t} = do
    let sp = [ "-fo", show o
             , "-t" , show t
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

---------------------------------------------------------

matchGPU err ratio ip1 ip2 = do
    let des1 = join (map ipDescriptor ip1)
        des2 = join (map ipDescriptor ip2)
    ptot <- malloc
    res <- createVector (2 * min (dim des1) (dim des2) `div` 128)
    app3 (c_matchGPU ptot err ratio) vec des1 vec des2 vec res "c_matchGPU"
    tot <- fromIntegral `fmap` peek ptot
    let mres = map (map round) $ toLists $ reshape 2 (subVector 0 (tot*2) res)
    free ptot
    if tot > 0 then return (mres :: [[Int]])
               else return []

foreign import ccall "c_matchGPU"
    c_matchGPU :: Ptr CInt -> Double -> Double -- tot, err, ratio
               -> CInt -> Ptr Double           -- v1
               -> CInt -> Ptr Double           -- v2
               -> CInt -> Ptr Double           -- res
               -> IO CInt

getMatchGPU = do
    w <- createWindow "aux MATCHGPU"
    windowStatus $= Hidden
    return $ \e r v1 v2 -> if null v1 || null v2
        then []
        else unsafePerformIO $ inContext w (matchGPU e r v1 v2)
