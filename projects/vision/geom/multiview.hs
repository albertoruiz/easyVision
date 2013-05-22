{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

import Vision.GUI
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((#),(&),row, col, diagl, norm)
import Util.Geometry
import Util.Camera
import Util.Small(unsafeMap)
import Vision.Camera
import Util.Misc(degree,median)
import Numeric.LinearAlgebra.Tensor hiding (scalar)
import Numeric.LinearAlgebra.Array.Util hiding (scalar)
import qualified Util.Homogeneous as H
import Util.Estimation(homogSolve,procrustes)
import Graphics.Plot(mplot)
import Numeric.LinearAlgebra.Array.Solve
import Vision.Stereo
import Vision.Tensorial--(eps3)

main = do
    info
    runIt $ browser3D "multiview" [drawing] (const id)
         >> browser3D "solution"  [soldraw] (const id)

info = do
    --mapM_ (disp.toMatrix) cams
    putStr "cameras " ; sht $ dummyAt 1 tcams
    --mapM_ print points
    putStr "points " ; sht tpoints
    putStr "views "; sht $ inhomogT "v" $ tviews~>"cnv"
    --sht $ dummyAt 1 $ dat 2 ~> "n12"
    putStr "Fundamental Matrix "; sht fun
    --sht $ dat 3 ~> "n123"
    putStr "Trifocal Tensor "; sht tri
    --sht $ dummyAt 1 $ dat 4 ~> "n1234"
    putStr "Quadrifocal Tensor "; sht qua
    putStr "reprojection error "; print $ quality sol
 
drawing = clearColor white [ color gray (axes3D 5)
                     , color red . pointSz 3 $ drawPoints3DLabeled points
                     , color green . pointSz 1 $ drawPoints3DLabeled camcenters
                     , draws (zipWith drcamps cams views)
                     ]

soldraw = clearColor white [ -- color gray (axes3D 5)
                       color red . pointSz 3 $ drawPoints3DLabeled solpoints
                     , color green . pointSz 1 $ drawPoints3DLabeled solcamcenters
--                     , draws (zipWith drcamps solcams solviews)
                     , draws (map drcam solcams)
                     ]


mkCam (Point3D cx cy cz) (Point3D x y z) = unsafeFromMatrix $ syntheticCamera $ easyCamera (60*degree) (cx,cy,cz) (x,y,z) 0 :: Camera

disp = putStr . dispf 5

sht t = (putStrLn . formatFixed 2) t

drcam c = color green $ showCam 0.5 (infoCam c) Nothing

drcamps c ps = color green [ showCam 0.5 ic Nothing
                           , pointSz 3 $ (toImagePlane ic 0.5) ps
                           ]
  where
    ic = infoCam c


cams = concatMap (\c->[c,theRight c]) (take 4 mycams)
  where
    mycams = zipWith mkCam [Point3D 2 4 4, Point3D 4 4 4, Point3D 1 2 3, Point3D 0 0 4]
                           [Point3D 1 0 0, Point3D 0 0 0, Point3D 0 0 0, Point3D 0 0 0]


points = 
    [ Point3D 0 2 (-1)
    , Point3D 0 1 (-1)
    , Point3D 0 0 (-1)
    , Point3D 0 2 0
    , Point3D 0 1 0
    , Point3D 0 0 0
    , Point3D 1 2 0
    , Point3D 1 1 0
    , Point3D 1 0 1  -- !! 
    ]

tcams = subindex "c" (map toTensor cams) !> "1v 2p"
tpoints = subindex "n" (map (toTensor.homog) points) !> "1p"
tviews = tcams * tpoints
views = map (map (unsafeFromVector.inhomog.coords) . flip parts "n") $ parts tviews "c" :: [[Point]]

dat k = outers (renameParts "c" ((take k `onIndex` "c") tviews) "v" "")
fun = solveH (dat 2) "12"
tri = solveH (dat 3 * eps3!"1pq" * eps3!"2rs") "pr3" !> "p1 r2"
qua = solveH (dat 4 * eps3!"1ab" * eps3!"2fg" * eps3!"3pq" * eps3!"4uv") "afpu" !> "a1 f2 p3 u4"

--solveHomog (dat 2) ["1","2"] (Left eps)  -- with last = 1 0 0 we get a degenerate sol for F  

fun' = estimateFundamental (f 0) (f 1)
  where
    f = map (toList.toVector). (views!!)

sol = zoomProb 3 $ flipDir $ relocate $ autoCalibrate Nothing $ fourViews 7777 tviews

solcams = map (unsafeFromMatrix.asMatrix) $ parts (Vision.Tensorial.cam sol) "c"
solpoints = map unsafeFromVector $ toRows $ asMatrix $ inhomogT "x" $ (Vision.Tensorial.p3d sol) :: [Point3D]
solcamcenters =  map (inhomog.camCenter.unsafeFromMatrix.asMatrix) (flip parts "c" $ Vision.Tensorial.cam sol)

zoomProb :: Double -> VProb -> VProb
zoomProb s' p = p { p3d = (p3d p * hi)!>"yx", Vision.Tensorial.cam = (Vision.Tensorial.cam p * h) !> "yx"}
  where
    s = recip s'
    hi = applyAsMatrix inv h !"yx"
    h = listTensor [4,-4] [s,0,0,0,
                           0,s,0,0,
                           0,0,s,0,
                           0,0,0, 1] !"xy"

--------------------------------------------------------------------------------

camcenters = map (inhomog.camCenter) cams

camCenter :: Camera -> HPoint3D
camCenter = unsafeFromVector . nullVector . toMatrix

