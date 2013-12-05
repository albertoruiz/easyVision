module Image (
    Image(), Size(..), ROI(..), Pixel(..), Point(..),
    I8u, I8u3, I32f, Word24(..),
    Gray, RGB, Channels(..),
    size, roi, modifyROI, setROI,
    saveImage, loadRGB,
    img2mat, mat2img,
) where

import Image.Core
import Image.Convert
import Util.Geometry(Point(..))

data Channels = CHIm
    { yuv  :: ImageYUV
    , yCh  :: Image I8u
    , uCh  :: Image I8u
    , vCh  :: Image I8u
    , rgb  :: Image Word24
    , rCh  :: Image I8u
    , gCh  :: Image I8u
    , bCh  :: Image I8u
    , hsv  :: Image Word24
    , hCh  :: Image I8u
    , sCh  :: Image I8u
    , fCh  :: Image Float
    }

