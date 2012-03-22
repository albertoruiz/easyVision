{-# LANGUAGE RecordWildCards #-}

-- additional tools for camera analysis and graphic representation
module Util.Camera (
    CameraInfo(..),
    infoCam,
    showCam,
    computeCamera
) where


import Vision.GUI
import ImagProc hiding (Pixel(..))
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((!),(#),row)
import Vision
import Util.Geometry
import Util.Estimation


-- provisional
computeCamera :: [Point] -> [Point3D] -> Camera
computeCamera image world = unsafeFromMatrix m
  where
    m = estimateCamera (map p2l image) (map p2l world)
    p2l x = toList . toVector $ x



auxImg = resize (Size 256 256) . float . grayscale . channelsFromRGB


data CameraInfo = CameraInfo
    { cam :: Camera -- original camera
    , kCam :: Homography
    , rtCam :: Camera
    , fCam :: Double
    , cenCam :: HPoint3D
    , toImagePlane :: Double -> [Point] -> [HPoint3D] --
    , cam2world :: Homography3D
    , world2cam :: Homography3D
    }


infoCam :: Camera -> CameraInfo
infoCam c = CameraInfo{..}
  where
    cam = c
    m = toMatrix c
    (k,rt) = sepCam m
    kCam = unsafeFromMatrix k
    rtCam = unsafeFromMatrix rt
    cenCam = unsafeFromVector (nullVector m)
    fCam = k@@>(0,0)
    world2cam = unsafeFromMatrix $ rt # row [0,0,0,1]
    cam2world = invTrans world2cam
    ik = unsafeFromMatrix $ inv k ! 0 # row [0,0,0,1] :: Homography3D 
    tip = cam2world ⊙ ik
    toImagePlane sc ps = tip ◁  map as3D ps
      where
        as3D (Point x y) = HPoint3D x y 1 (recip sc)


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

showCam sc ic@CameraInfo{..} Nothing = Draw [ wireCamera ic sc 1
                                            , lineStrip (calibFrame ic sc)]
  

showCam sc ic (Just img) = Draw [ wireCamera ic sc (aspectRatio (size img))
                                , drawImagePlane sc ic img
                                , lineStrip (calibFrame ic sc) ]


