{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.NP
Copyright   :  (c) Pedro E. Lopez de Teruel and Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Interface to the New Paradigm.

-}
-----------------------------------------------------------------------------

module ImagProc.C.NP (   
    wnpcontours, argNPParam, defNPParam, winNPParam
)
where

import ImagProc.Ipp.Core
import ImagProc
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import EasyVision.GUI.Parameters(autoParam,intParam,realParam)
import Contours.Base(douglasPeuckerClosed,douglasPeucker)
import EasyVision.GUI((.@.))
import Control.Monad(when)
import Data.Packed.Development(createVector, app1, vec)
import Data.Packed(dim,toList,takesV,Vector,(@>))
import System.CPUTime
import Data.Time.Clock
import Text.Printf

--------------------------------------------------------------------------------

foreign import ccall unsafe "get_contours"
   c_npb3 :: Ptr () -> CInt                              -- ptr step
          -> CInt -> CInt -> CInt -> CInt                -- c1 c2 r1 r2
          -> CInt -> CInt                                -- cols rows
          -> CInt -> CInt                                -- th minlen
          -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)            -- ps cs
          -> Ptr CInt -> Ptr CInt                        -- np nc
          -> CInt                                        -- w/alloca
          -> IO Int

foreign import ccall unsafe "pack_contours"
   c_pack :: Ptr CInt -> Ptr CInt                        -- ips ics
          -> CInt -> CInt                                -- inp inc
          -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)            -- ops ocs
          -> Ptr CInt -> Ptr CInt                        -- onp onc
          -> IO Int

--------------------------------------------------------------------------------

autoParam "NPParam" ""
    [ ("redu", "Double", realParam 1.5 0 10)
    , ("th","Int",intParam 10 0 100)
    , ("minlen", "Int", intParam 50 0 200)
--  , ("wAlloca", "Int", intParam 0 0 1)
    ]

wnpcontours :: (x -> ImageGray) -> IO x -> IO (IO (x, (([Polyline],[Polyline]),[Polyline])))
wnpcontours f = (\p -> npcontours p . f) .@. winNPParam

argNPParam :: IO NPParam
defNPParam :: NPParam
winNPParam :: IO (IO NPParam)

---------------------------------------

npbRaw :: Int -> Int -> Int -> ImageGray -> [(CInt, Vector CInt)]
npbRaw th minlen w_alloca x = unsafePerformIO $ do
  let G im = x
      v z = fi . z . vroi $ im
  
  rppp <- new nullPtr
  rppc <- new nullPtr
  rpn <- new 0
  rcn <- new 0

  t0 <- getCurrentTime

  r1 <- c_npb3 (ptr im) (fi.step $ im)
               (v c1) (v c2) (v r1) (v r2)
               (fi.width.isize $ im) (fi.height.isize $ im)
               (fi th) (fi minlen)
               rppp rppc rpn rcn
               (fi w_alloca)    

  t1 <- getCurrentTime
  --print (diffUTCTime t1 t0)
  
  rpp <- peek rppp
  rpc <- peek rppc
  rp  <- peek rpn
  rc  <- peek rcn

  ppp <- new nullPtr
  ppc <- new nullPtr
  pn <- new 0
  cn <- new 0

  r2 <- c_pack rpp rpc rp rc ppp ppc pn cn

  pp <- peek ppp
  pc <- peek ppc
  c <- peek cn >>= flip peekArray pc . ti
  n <- ti `fmap` peek pn

  --print (rp,n)

  let c' = map ((2*).abs.ti) c
  res <- if n == 0
            then return []
            else do
                p <- createVector n
                app1 (\_ d -> copyArray d pp n >> return 0) vec p "memcopy npbRaw"
                return (takesV c' p)
  
  mapM_ free [pn,cn,pp,pc,rpn,rcn,rpp,rpc]
  mapM_ free [ppp,ppc,rppp,rppc]
  touchForeignPtr.fptr $ im
  return $ zip c res


npbParse :: [(CInt, Vector CInt)] -> ([[Pixel]],[[Pixel]],[[Pixel]])  -- op clb clw
npbParse = go [] [] []
  where
    go op clb clw [] = (op,clb,clw)
    go op clb clw ((c,pts):xs) | closed pts =
        if c > 0 then go op (toPixels pts:clb) clw xs
                 else go op clb (toPixels pts:clw) xs
    go op clb clw ((c,pts):xs) = go (toPixels pts:op) clb clw xs 

    closed p = abs(x1-x2) + abs(y1-y2) <= 1
      where
        x1 = p@>0; y1 = p@>1
        x2 = p@>(dim p - 2); y2 = p@>(dim p -1)

    toPixels = toPix . toList
    toPix [] = []
    toPix (x:y:zs) = Pixel (ti y) (ti x) : toPix zs

-- | result = (closed, open)
npcontours :: NPParam -> ImageGray -> (([Polyline],[Polyline]),[Polyline])

npcontours NPParam{..} x = ((map proc1 clb, map proc1 clw), map proc2 op)
  where
    (op,clb,clw) = npbParse . npbRaw th minlen wAlloca $ modifyROI (shrink (5,5)) x
    proc1 = Closed . pixelsToPoints (size x) . douglasPeuckerClosed redu
    proc2 = Open . pixelsToPoints (size x) . douglasPeucker redu
    wAlloca = 0 -- TO DO: change to auto
