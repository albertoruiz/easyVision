{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.Contours
Copyright   :  PARP Research Group, University of Murcia, Spain, 2012
License     :  All rights reserved

Maintainer  :  Alberto Ruiz (aruiz at um dot es)

Interface to optimized C implementation of subpixel contour extraction.

-}
-----------------------------------------------------------------------------

module ImagProc.Contrib.Contours (
    npContours, NPParam,
    compute_HoG
) where

import ImagProc.Ipp.Core
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal hiding (alloca)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import Vision.GUI.Parameters
import Data.Packed.Development(createVector, app1, vec)
import Numeric.LinearAlgebra hiding (step)
import ImagProc.Contrib.Contours.Structs
import Control.Monad(when)
--import Util.Misc(debug)

--------------------------------------------------------------------------------

foreign import ccall unsafe "get_contours" get_contours
    :: Ptr () -> CInt                              -- ptr step
    -> CInt -> CInt -> CInt -> CInt                -- c1 c2 r1 r2
    -> CInt -> CInt                                -- cols rows
    -> CInt -> CInt                                -- th minlen
    -> Ptr TContours                               -- ptr contours
    -> CInt                                        -- options
    -> IO Int

foreign import ccall unsafe "pack_contours" pack_contours
    :: Ptr TContours                               -- ptr contours
    -> Float                                       -- pack threshold
    -> IO CInt

foreign import ccall unsafe "free_mem_contours" free_mem_contours
    :: Ptr TContours
    -> IO ()

foreign import ccall unsafe "compute_HoG" compute_HoG
    :: Ptr TContours -> CInt -> CInt -> Ptr Float
    -> IO ()

--------------------------------------------------------------------------------

rawContours :: Int -> Int -> CInt -> Bool -> Float -> ImageGray
            -> [(CInt, (Vector Float, Vector Float))]
rawContours th minlen options pack thPack imgSrc = unsafePerformIO $ do
    let G im = modifyROI (shrink (5,5)) imgSrc
        v z = fi . z . vroi $ im

    ptrConts <- malloc

    --putStrLn "entro"
    _ok <- get_contours (ptr im) (fi.step $ im) (v c1) (v c2) (v r1) (v r2)
                        (fi.width.isize $ im) (fi.height.isize $ im)
                        (fi th) (fi minlen)
                        ptrConts
                        options
    --putStrLn "salgo"

    when pack $ do
        --putStrLn "pack..."
        _okp <- pack_contours ptrConts thPack
        return ()

    conts <- peek ptrConts

    c <- peekArray (ti (cn conts)) (cs conts)

    let n = ti (pn conts)

    let c' = map (abs.ti) c
        --n = sum c'

    --print c
    --print c'
    --print (n, oldn `div` 2)

    (resx, resy) <- if n == 0
            then return ([],[])
            else do
                xs <- createVector n :: IO (Vector Float)
                ys <- createVector n :: IO (Vector Float)
                --putStrLn "vectors created"
                app1 (\_ d -> copyArray d (x conts) n >> return 0) vec xs "memcopy npbRaw x"
                app1 (\_ d -> copyArray d (y conts) n >> return 0) vec ys "memcopy npbRaw y"
                --putStrLn "vectors copied";  print (dim xs, dim ys)
                return (takesV c' xs, takesV c' ys)

    touchForeignPtr.fptr $ im

    free_mem_contours ptrConts
    free (ptrConts)

    return $ (zip c (zip resx resy))

autoParam "NPParam" "npc-"
    [ ("thres",    "Int",   intParam 10 0 100)
    , ("pack",     "Int",   intParam 1 0 1)
    , ("thPack",   "Float", realParam 0.5 0 3)
    , ("minlen",   "Int",   intParam 50 0 200)
    , ("alloca",   "Int",   intParam 0 0 1)
    , ("open",     "Int",   intParam 1 0 1)
    , ("dark",     "Int",   intParam 1 0 1)
    , ("light",    "Int",   intParam 1 0 1)
    , ("subpixel", "Int",   intParam 1 0 1)
    , ("smooth",   "Int",   intParam 1 0 1) ]


mkNPParam :: MkParam NPParam
defNPParam :: NPParam
argNPParam :: IO NPParam

rawParse :: Size -> [(CInt, (Vector Float, Vector Float))] -> (([Matrix Float],[Matrix Float]),[Matrix Float])
rawParse sz = go [] [] []
  where
    go op clb clw [] = ((tm clb, tm clw), tm op)
    go op clb clw ((c,pts):xs) | closed pts =
        if c > 0 then go op (pts:clb) clw xs
                 else go op clb (pts:clw) xs
    go op clb clw ((_c,pts):xs) = go (pts:op) clb clw xs

    closed (xs,ys) = x1==x2 && y1==y2
      where
        x1 = xs@>0
        x2 = xs@>(dim xs -1)
        y1 = ys@>0
        y2 = ys@>(dim ys -1)

    tm = map cont2Mat

    cont2Mat (xs,ys) = m <> trans h
      where
        h = single $ pixelToPointTrans sz
        m = fromColumns [xs,ys,1]

getOptions :: NPParam -> Int
getOptions NPParam {..} = sum $ zipWith (*) [alloca, open, dark, light, subpixel, smooth] (map (2^) [0..5::Int])


npContours :: NPParam -> ImageGray -> (([Polyline], [Polyline]), [Polyline])
-- ^ subpixel contour extraction ((dark, light), open)
npContours p@NPParam {..} x = asPolylines
                            . rawParse (size x)
                            . rawContours thres minlen (fi $ getOptions p) (pack==1) thPack
                            $ x
  where
    asPolylines ((clb',clw'),op') = ((clb,clw),op)
      where
        clb = map (Closed . tail . matToPoly) (protect 4 clb')
        clw = map (Closed . tail . matToPoly) (protect 4 clw')
        op  = map (Open          . matToPoly) (protect 2 op')

        matToPoly = map v2p . toRows . double -- . spy
          where
            v2p v = Point (v@>0) (v@>1)

        protect n = filter ((>= n).rows)

-- spy m = if rows m < 3 then debug "R" rows m else m

