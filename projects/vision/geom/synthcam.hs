import Vision.GUI
import Util.Geometry
import Vision
import Util.Camera
import Numeric.LinearAlgebra.HMatrix

-- c = syntheticCamera $ easyCamera (pi/3) (4,4,1) (-1,2,5) 0
c = syntheticCamera $ easyCamera (pi/3) (0,-4,5) (0,0,0) 0
-- c = syntheticCamera $ easyCamera (pi/3) (0,0,5) (0,0,0) 0


(k,rt) = sepCam c

r = takeColumns 3 rt
t = last (toColumns rt)

ic = infoCam (unsafeFromMatrix c)

p0 = inhomog $ cenCam ic
[p1,p2,p3] = map (\r -> unsafeFromVector (r+toVector p0)) (toRows r)

ipts = toImagePlane ic 1 [Point 1.2 0, Point 0 1.2]


main = do
    disp 3 $ c
    disp 3 $ k
    disp 3 $ r
    disp 3 $ asRow t
    runIt $ browser3D "synth camera" [drw] (const Draw)

drw = clearColor white
    [ color black $ axes3D 4
    , color red [ showCamera 1 ic Nothing
                , (text3DAtF Helvetica12 (inhomog $ ipts!!0) "x")
                , (text3DAtF Helvetica12 (inhomog $ ipts!!1) "y")
                ]
    , color blue $ lineWd 3 $ lineStrip [ p0,p1,p0,p2,p0,p3 ]
    ]

