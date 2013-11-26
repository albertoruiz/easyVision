{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types      #-}

module Image.Core (
    Size(..), ROI(..), Pixel(..), Image(..),
    newImage,
    cloneImage,
    withImage,
    ptrAt,
    starting, rowPtrs,
    modifyROI, setROI,
    Word8, Word16, Word24(..),
    Gray, RGB, YCbCr, YUV,
    ImageGray, ImageFloat, ImageRGB, ImageYCbCr, ImageYUV,
    STImage, thawImage, runSTImage, ioRead, ioWrite, stRead, stWrite, readPixel,
    module Image.Types,
    module Image.ROI
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.ByteString(ByteString)
import Foreign.Ptr(alignPtr,minusPtr,Ptr,castPtr,plusPtr)
import GHC.ForeignPtr(mallocPlainForeignPtrBytes)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.ForeignPtr(withForeignPtr)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Storable(Storable(..))
import Foreign(Word16,Word8)
import Foreign.C.Types(CInt(..))
import Control.Monad.ST(ST, runST)
import Control.Monad.ST.Unsafe(unsafeIOToST)
import Util.Misc((//))
import Control.Monad(when)
import Image.Types
import Image.ROI



data Image t = Image
    { size  :: Size
    , roi   :: ROI
    , bytes :: ByteString
    , step  :: Int
    , szpix :: Int
    }

data Word24 = Word24 {-# UNPACK #-} !Word8
                     {-# UNPACK #-} !Word8
                     {-# UNPACK #-} !Word8

instance Storable Word24 where
  sizeOf _ = 3
  alignment _ = alignment (undefined::Float)
  peek p = do
    let pb = castPtr p
    [w1,w2,w3] <- mapM (peekElemOff pb) [0,1,2]
    return (Word24 w1 w2 w3)
  poke p (Word24 w1 w2 w3) = do
    let pb = castPtr p
    pokeElemOff pb 0 w1
    pokeElemOff pb 1 w2
    pokeElemOff pb 2 w3

type RGB   = Word24
type Gray  = Word8
type YCbCr = Word16

type ImageGray  = Image Gray
type ImageRGB   = Image RGB
type ImageFloat = Image Float
type ImageYCbCr = Image YCbCr

type YUV = Word16
type ImageYUV = Image YUV



ptrAt :: Image t -> Pixel -> Ptr t
ptrAt Image{..} (Pixel r c) = castPtr $ unsafeForeignPtrToPtr fp `plusPtr` (o + r*step + c*szpix)
  where
    B.PS fp o _ = bytes


starting :: Image t -> Ptr Word8
starting x = castPtr $ ptrAt x (Pixel r1 c1)
  where
    ROI r1 _ c1 _ = roi x


rowPtrs :: Image t -> ([Ptr Word8],Int)
rowPtrs img@Image {..} = (map row [0..r2-r1], c2-c1+1)
  where
    ROI r1 r2 c1 c2 = roi
    p = starting img
    row k = plusPtr p (k*step)


withImage :: Image t -> IO b -> IO b
withImage Image{..} act = withForeignPtr fp $ \_ -> act
  where
    B.PS fp _ _ = bytes


newImage :: Storable t => t -> Size -> IO (Image t)
newImage z sz@(Size h w) = do
    when (h < 1 || w < 1) $ error ("newImage "++ show sz)
    let sp = sizeOf z
    (bs,s) <- alignedBytes sp sz
    return Image {size = sz, roi = fullROI sz, bytes = bs, step = s, szpix = sp }


cloneImage :: Image t -> IO (Image t)
cloneImage x = return $ x { bytes = B.copy (bytes x) }


alignedBytes :: Int -> Size -> IO (ByteString,Int)
alignedBytes sz (Size r c) = do
    let w = c*sz
        rest = w `mod` 32
        c' = if rest == 0 then w else w + 32 - rest
        tl = r*c'+31
    fp <- mallocPlainForeignPtrBytes tl
    let p' = unsafeForeignPtrToPtr fp
        p = alignPtr p' 32
        o = p `minusPtr` p'
        t = tl - o
    return (B.PS fp o t, c')


--------------------------------------------------------------------------------

modifyROI :: (ROI->ROI) -> Image p -> Image p
modifyROI f im = im { roi = f (roi im) `intersection` (roi im) }

setROI :: ROI -> Image p -> Image p
setROI r = modifyROI (const r)

--------------------------------------------------------------------------------

newtype STImage s t = STImage (Image t)

thawImage :: Image t -> ST s (STImage s t)
thawImage = unsafeIOToST . fmap STImage . cloneImage

freezeImage :: (Storable t) => STImage s1 t -> ST s2 (Image t)
freezeImage (STImage x) = unsafeIOToST . return $ x

runSTImage :: Storable t => (forall s . ST s (STImage s t)) -> Image t
runSTImage st = runST (st >>= freezeImage)

{-# INLINE ioRead #-}
ioRead :: Storable t => Image t -> Pixel -> IO t
ioRead x p = withImage x $ peek (ptrAt x p)

{-# INLINE readPixel #-}
readPixel :: Storable a => Pixel -> Image a -> a
readPixel p x = B.inlinePerformIO (ioRead x p)

{-# INLINE ioWrite #-}
ioWrite :: Storable t => Image t -> Pixel -> t -> IO ()
ioWrite x p v = withImage x $ poke (ptrAt x p) v

{-# INLINE stRead #-}
stRead :: Storable t => STImage s t -> Pixel -> ST s t
stRead (STImage x) p = unsafeIOToST $ ioRead x p

{-# INLINE stWrite #-}
stWrite :: Storable t => STImage s t -> Pixel -> t -> ST s ()
stWrite (STImage x) p v = unsafeIOToST $ ioWrite x p v

--------------------------------------------------------------------------------

