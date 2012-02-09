{-# LANGUAGE ForeignFunctionInterface #-}
module Module where
 
import Foreign.C.Types
 
hsfun :: CInt -> IO CInt
hsfun x = do
    putStrLn "Hello World"
    return (42 + x)
 
foreign export ccall
    hsfun :: CInt -> IO CInt

