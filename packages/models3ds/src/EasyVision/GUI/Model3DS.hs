{-# LANGUAGE ForeignFunctionInterface #-}

module EasyVision.GUI.Model3DS(
    loadModel,
    storeModel,
    render3ds,
    prepareModel
) where

import Foreign.Ptr
import Foreign.C.String
import Control.Applicative((<$>))

newtype ModelFile = ModelFile { modelFile :: Ptr () }
newtype ModelData = ModelData { modelData :: Ptr () }

loadModel :: FilePath -> IO ModelFile
loadModel name = withCString name (fmap ModelFile . load3ds_c)

foreign import ccall "load3ds" load3ds_c :: CString -> IO (Ptr ())

storeModel :: ModelFile -> IO ModelData
storeModel =  fmap ModelData . store3ds_c . modelFile

foreign import ccall "store3ds" store3ds_c :: Ptr () -> IO (Ptr())

render3ds :: ModelData -> IO ()
render3ds = render3ds_c . modelData

foreign import ccall "renderit" render3ds_c :: Ptr () -> IO ()

prepareModel :: String -> IO (IO ()) 
prepareModel name = do
    m <- loadModel name >>= storeModel
    return (render3ds m)
 
