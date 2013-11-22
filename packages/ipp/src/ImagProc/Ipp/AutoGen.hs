{-# OPTIONS #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.AutoGen
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Generators of Haskell style functions from IPP wrappers

-}
-----------------------------------------------------------------------------

module ImagProc.Ipp.AutoGen(
    auto_0_8u_C1R,
    auto_0_8u_C3R,
    auto_0_32f_C1R,

    auto_1_8u_C1R,
    auto_1_8u_C3R,
    auto_1_32f_C1R,
    auto_1_8u_C3C1R,
    auto_1_8u_C3C2R,
    auto_1_8u32f_C1R,
    auto_1_32f8u_C1R,

    auto_1_8u_P3C3R,
    auto_1_8u_C3P3R,
    auto_1_8u_P3R,
    auto_1_8u_C1C3R,
    auto_1_8u_C2C3R,
    auto_1_32f_C1MR,
    auto_1_8u_C1MR,
    auto_1_8u_C1RSfs,

    auto_2_8u_C1R,
    auto_2_32f_C1R,
    auto_2_8u_C1RSfs,

    auto_11_8u_C1IR,
    auto_11_32f_C1IR,

    auto_1_8u32f64f_C1R

) where

import ImagProc.Ipp.Core

--------------------------------------------------------------------------------

type Auto_0 t = Dst t (IO CInt) -> String -> Size -> IO (Image t)

auto_0 :: Storable t => Dst t (IO CInt) -> String -> Size -> IO (Image t)
auto_0 f msg sz = do
    r <- newImage undefined sz
    f // dst r // checkIPP msg
    return r

auto_0_8u_C1R :: Auto_0 Word8
auto_0_8u_C1R = auto_0

auto_0_8u_C3R :: Auto_0 Word24
auto_0_8u_C3R = auto_0

auto_0_32f_C1R :: Auto_0 Float
auto_0_32f_C1R = auto_0

--------------------------------------------------------------------------------

newImageAsR1 :: Storable q => (ROI->ROI) -> Image p -> IO (Image q)
newImageAsR1 roifun im = do
    r <- newImage undefined (size im)
    return $ setROI (roifun (roi im)) r

cr1 :: Src p (Dst q (IO CInt)) -> String -> Image p -> Image q -> IO ()
cr1 f msg im r = withImage im $ do
    f // src im (roi r) // dst r // checkIPP msg

type Auto_1 p q = Src p (Dst q (IO CInt)) -> String -> (ROI -> ROI) -> Image p -> IO (Image q)

auto_1 :: (Storable p, Storable q) => Auto_1 p q
auto_1 f msg roifun im = do
    r <- newImageAsR1 roifun im
    cr1 f msg im r
    return r

auto_1_8u_C1R :: Auto_1 Word8 Word8
auto_1_8u_C1R = auto_1

auto_1_8u_C3R :: Auto_1 Word24 Word24
auto_1_8u_C3R = auto_1

auto_1_32f_C1R :: Auto_1 Float Float
auto_1_32f_C1R = auto_1

auto_1_8u_C3C1R :: Auto_1 Word24 Word8
auto_1_8u_C3C1R = auto_1

auto_1_8u_C3C2R :: Auto_1 Word24 Word16
auto_1_8u_C3C2R = auto_1

auto_1_8u_C1C3R :: Auto_1 Word8 Word24
auto_1_8u_C1C3R = auto_1

auto_1_8u_C2C3R :: Auto_1 Word16 Word24
auto_1_8u_C2C3R = auto_1

auto_1_8u32f_C1R :: Auto_1 Word8 Float
auto_1_8u32f_C1R = auto_1

auto_1_32f8u_C1R :: Auto_1 Float Word8
auto_1_32f8u_C1R = auto_1

auto_1_8u_C1RSfs :: Auto_1 Word8 Word8
auto_1_8u_C1RSfs = auto_1_8u_C1R

auto_1_8u_P3C3R = error $ "auto_1_8u_P3C3R not yet defined"
auto_1_8u_C3P3R = error $ "auto_1_8u_C3P3R not yet defined"
auto_1_8u_P3R = error $ "auto_1_8u_P3R not yet defined"
auto_1_32f_C1MR = error $ "auto_1_32f_C1MR not yet defined"
auto_1_8u_C1MR = error $ "auto_1_8u_C1MR not yet defined"

auto_1_8u32f64f_C1R = error "FIXME auto_1_8u32f64f_C1R"

{-

cr12 f msg im r1 r2 = f // src im (vroi r2) // src r1 (vroi r2) // dst r2 (vroi r2) // checkIPP msg [im]

auto_1_8u32f64f_C1R f msg roifun (G im) = do
    r' <- img I32f (isize im)
    let r = r' { vroi = roifun (vroi im) }
    s' <- img I64f (isize im)
    let s = s' { vroi = roifun (vroi im) }
    cr12 f msg im r s
    return (F r, D s)

-}

--------------------------------------------------------------------------------

newImageAsR2 :: Storable p => (ROI->ROI->ROI) -> Image p -> Image p -> IO (Image p)
newImageAsR2 roifun im1 im2 = do
    r <- newImage undefined (size im1)
    return $ setROI (roifun (roi im1) (roi im2)) r

type Auto_2 p =  Src p (Src p (Dst p (IO CInt))) -> String
              -> (ROI-> ROI -> ROI) 
              -> (ROI-> ROI -> ROI)
              -> (ROI-> ROI -> ROI)
              -> Image p -> Image p -> IO (Image p)

cr2 :: Src p (Src q (Dst r (IO CInt))) -> String
    -> Image p -> Image q -> Image r
    -> IO ()
cr2 f msg im1 im2 r = withImage im1 $ withImage im2 $ do
    f // src im1 (roi im1) // src im2 (roi im2)// dst r // checkIPP msg

auto_2 :: Storable p => Auto_2 p
auto_2 f msg rf rf1 rf2 im1 im2 = do
    r <- newImageAsR2 rf im1 im2
    let im1' = setROI (rf1 (roi im1) (roi im2)) im1
        im2' = setROI (rf2 (roi im1) (roi im2)) im2
    cr2 f msg im1 im2 r
    return r

auto_2_8u_C1R :: Auto_2 Word8
auto_2_8u_C1R = auto_2

auto_2_32f_C1R :: Auto_2 Float
auto_2_32f_C1R = auto_2

auto_2_8u_C1RSfs :: Auto_2 Word8
auto_2_8u_C1RSfs = auto_2_8u_C1R

{-

------------------------------------------------------------

-- in place, roifun not used, using the rois of the inputs

cr2i f msg im1 im2 = f // src im1 (vroi im1) // dst im2 (vroi im2) // checkIPP msg [im1,im2]

auto_11_32f_C1IR f msg _ (F im1) (F im2) = do
    cr2i f msg im1 im2

auto_11_8u_C1IR f msg _ (G im1) (G im2) = do
    cr2i f msg im1 im2
    
-}

auto_11_32f_C1IR = error $ "FIXME auto_11_32f_C1IR"
auto_11_8u_C1IR = error $ "FIXME auto_11_8u_C1IR"

