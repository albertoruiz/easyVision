-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP.AdHoc
Copyright   :  (c) Alberto Ruiz 2006-15
License     :  GPL

Maintainer  :  Alberto Ruiz <aruiz@um.es>
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Processing.IPP.AdHoc(
    resize8u,resize8u3,resize32f
)
where


import Image.Processing.IPP.Core
import Image.Processing.IPP.Adapt
import Image.Processing.IPP.Wrappers
import Image.Processing.IPP.Auto
import Foreign.Marshal
import Control.Monad(when)
import Data.ByteString.Internal as B
import Foreign.Ptr(plusPtr,castPtr)
import Foreign.Storable(peek)
import Control.Applicative((<$>))


--------------------------------------------------------------------------------

resizeg :: Storable p => String -> RawImage p (RawImage p (IO CInt))
        -> Size -> Image p -> Image p
resizeg msg f s im = unsafePerformIO $ do
    when (roiArea (fullROI s) <=0) $ error $ "resize result " ++ show s
    r <- newImage undefined s
    withImage im $ checkIPP msg $ do
        f `appI` im `appI` r
    return r

resize32f :: Size -> Image Float -> Image Float
resize32f = resizeg "resize32f" c_resize32f

resize8u :: Size -> Image Word8 -> Image Word8
resize8u = resizeg "resize8u" c_resize8u

resize8u3 :: Size -> Image Word24 -> Image Word24
resize8u3 = resizeg "resize8u3" c_resize8u3


