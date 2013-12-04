import Vision.GUI
import Image.Processing
import Vision.Camera
import Util.Options(getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((!),(#),row, col, diagl)
import Util.Geometry
import Util.Camera

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    runIt $ do
        p <- clickPoints "click points" "--points" () (sh mbimg . fst)
        w <- browser3D "camera resection" [] (const id)
        connectWith (g mbimg) p w

sh mbimg pts = Draw [ Draw $ fmap (rgb.channelsFromRGB) mbimg
                    , color lightgreen . drawPointsLabeled $ pts
                    , lineWd 2 . color blue $ lineStrip $ cam  ◁  mycube1
                    , lineWd 2 . color violet  $ lineStrip $ cam  ◁  mycube2
                    , lineWd 2 . color green  $ lineStrip $ cam  ◁  mycube3 ]
  where
    cam = computeCamera pts ref

g mbimg (n,_) (ps,_) = (n, [ clearColor white
                              [ color gray $ axes3D 4
                              , color red . pointSz 3 $ drawPoints3DLabeled ref
                              , drcam
                              , color blue (lineStrip mycube1)
                              , color violet (lineStrip mycube2)
                              , color green (lineStrip mycube3)
                              , pointSz 5 . color orange $ ipts
                              , color lightgray rays
                              ]
                           ])
  where
    drcam | length ps < length ref = Draw ()
          | otherwise = color green $ showCamera 2 ic (fmap (toFloat.grayscale.channelsFromRGB) mbimg)
    cam = computeCamera ps ref
    ic = infoCam cam 
    ipts = toImagePlane ic 2 ps
    invc = invTrans cam
    rays = invc <| map homog ps

ref = [Point3D x 0 z | z <- [0,1,2,3], x <- [0,1,2,3] ]
    ++ [Point3D x y 3 | y <- [1,2,3],   x <- [0,1,2,3] ]
    ++ [Point3D 0 y z | z <- [2,1,0],   y <- [3,2,1] ]

mycube1 = despla 1 1 3 ◁ cube
mycube2 = despla 1 (-1) 1 ◁ cube
mycube3 = despla (-1) 1 1 ◁ cube

scaleit :: Double -> Homography3D
scaleit s = unsafeFromMatrix $ diagl [s,s,s,1] 

despla :: Double -> Double -> Double -> Homography3D
despla x y z = unsafeFromMatrix $ 
    ident 3 ! col[x,y,z]
    # row [0,0,0,1]

cube = [p1,p2,p3,p4,p1
       ,p5,p6,p7,p8,p5
       ,p6,p2,p3,p7,p8,p4]
  where
    p1 = Point3D 1 0 0
    p2 = Point3D 1 1 0
    p3 = Point3D 0 1 0
    p4 = Point3D 0 0 0    
    p5 = Point3D 1 0 1
    p6 = Point3D 1 1 1
    p7 = Point3D 0 1 1
    p8 = Point3D 0 0 1

