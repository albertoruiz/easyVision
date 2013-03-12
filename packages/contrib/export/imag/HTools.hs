{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(fun, hfun,
              funInit, hfunInit) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Marshal.Array(copyArray)
import ImagProc
import ImagProc.Ipp.Core
import GHC.Stable

import Numeric.LinearAlgebra

--------------------------------------------------------------------------------

-- function with arguments, some of them typically provided by initialization

funInit :: HInit
funInit s = loadMatrix s

fun :: HFun
fun m k img | k == 0 = img
            | k == 1 = notI img
            | k == 2 = warp 128 (size img) m img
            | otherwise = error ("funpar: unknown code " ++ show k)

--------------------------------------------------------------------------------

(hfunInit, hfun) = mkC (funInit, fun)
foreign export ccall hfun :: CFun
foreign export ccall hfunInit :: CInit

--------------------------------------------------------------------------------

type HInit = String -> IO (Matrix Double)
type CInit = CString -> IO (Ptr ())

type HFun = Matrix Double -> Int -> (ImageGray -> ImageGray)

type CFun =  Ptr ()                      -- intialized argument
          -> CInt                        -- normal argument
          -> Ptr CUChar -> CInt -> CInt  -- source and size (rows columns)
          -> Ptr CUChar                  -- destination
          -> IO Int                      -- exit code

--------------------------------------------------------------------------------

mkC :: (HInit, HFun) -> (CInit, CFun)
mkC (init, f) = (cInit, cFun)
  where
    cInit ps = do
      s <- peekCString ps
      a <- init s
      p <- newStablePtr a
      return (castStablePtrToPtr p)

    cFun x k pS sr sc pD = do
      let r = ti sr; c = ti sc
      img @ (G Img {ptr = p}) <- image (Size r c)
      copyArray (castPtr p) pS (r*c)
    
      a <- deRefStablePtr (castPtrToStablePtr x)
    
      let G Img {ptr = d} = f a (ti k) img

      copyArray pD (castPtr d) (r*c)
      return 0

