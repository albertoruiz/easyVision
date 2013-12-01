{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.Examples
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module ImagProc.Contrib.Examples (
    sumInC, invertInC
)
where

import Image.Devel
import Foreign.Marshal ( malloc, free )
import Foreign.Storable ( Storable(peek) )

----------------------------------------------------

foreign import ccall "customSum"
    c_customSum :: Ptr CInt -> RawImage I8u (IO CInt)

sumInC :: Image I8u -> Int
sumInC x = ti . unsafePerformIO $ do
    presult <- malloc
    withImage x $ do
        c_customSum presult `appI` x // checkFFI "sumInC"
    result <- peek presult
    free presult
    return result 

------------------------------------------------

foreign import ccall "customInvert"
    c_customInvert :: RawImage I8u (RawImage I8u (IO CInt))
invertInC :: Image I8u -> Image I8u
invertInC = wrap11 "invertInC" c_customInvert

----------------------------------------------------

