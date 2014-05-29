{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module Vision.Apps.ShCamera (
    showCamera
) where


import Vision.GUI.Simple
import Image(Image,Size(..),size)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((¦),(#),row,norm,diagl)
import Image.Processing(resize)
import Vision.Camera(sepCam)
import Util.Geometry
import Util.Estimation
import Data.Function(on)
import Util.Misc(debugMat,debug,degree,median,Mat)
import Util.Rotation
import Util.Camera(CameraInfo(..),infoCam)

type ImageFloat = Image Float

auxImg = resize (Size 256 256) -- . float . grayscale . channelsFromRGB


aspectRatio (Size r c) = fromIntegral c / fromIntegral r :: Double


drawImagePlane sc caminf img = drawTexture (auxImg img) ps
  where
    ar = aspectRatio (size img)
    ps = map (toList . toVector . inhomog) (imageFrame caminf sc ar)


-- scaling aspectRatio
imageFrame :: CameraInfo -> Double -> Double -> [HPoint3D]
imageFrame CameraInfo{..} sc ar = tc ps
  where
    tc = toImagePlane sc
    x = 1
    y = recip ar
    ps = [ Point x y
         , Point (-x) y
         , Point (-x) (-y)
         , Point x (-y)
         ]

-- scaling aspectRatio
calibFrame :: CameraInfo -> Double -> [HPoint3D]
calibFrame CameraInfo{..} sc = cam2world <| ps
  where
    x = 1
    y = x
    z = 0.99
    w = recip sc
    ps = [ HPoint3D x    0    z w
         , HPoint3D (-x) 0    z w
         , HPoint3D 0    0    z w
         , HPoint3D 0    y    z w
         , HPoint3D 0    (-y) z w
         , HPoint3D x    (-y) z w
         , HPoint3D x    y    z w
         , HPoint3D (-x) y    z w
         , HPoint3D (-x) (-y) z w
         , HPoint3D x    (-y) z w
         , HPoint3D x    y    z w ]

wireCamera ic sc ar = lineStrip ps
  where
    [p1,p2,p3,p4] = imageFrame ic sc ar
    c = cenCam ic
    [x,y,z] = cam2world ic <| [HPoint3D sc 0 0 1, HPoint3D 0 sc 0 1, HPoint3D 0 0 (2*sc) 1]
    ps = [ -- x,c,y,c,z,
          z, c,p1,p2,p3,p4,p1,c,p2,p3,c,p4]

showCamera :: Double -> CameraInfo -> Maybe ImageFloat -> Drawing

showCamera sc ic@CameraInfo{..} Nothing
    = Draw [ wireCamera ic sc 1
           , lineStrip (calibFrame ic sc)
           ]
  
showCamera sc ic (Just img)
    = Draw [ wireCamera ic sc (aspectRatio (size img))
           , drawImagePlane sc ic img
           , lineStrip (calibFrame ic sc)
           ]

