{-# LANGUAGE ForeignFunctionInterface #-}

module HTools where

import Foreign.C.Types
import Numeric.LinearAlgebra

hsfun :: CInt -> IO CInt
hsfun x = do
    putStrLn "Hello World"
    return (42 + x)

hf2 :: Double -> Double
hf2 x = eigenvaluesSH m @> 0
  where
    m = (2><2) [ 1   , x
               , x   , x**2]

foreign export ccall hsfun :: CInt -> IO CInt
foreign export ccall hf2 :: Double -> Double

