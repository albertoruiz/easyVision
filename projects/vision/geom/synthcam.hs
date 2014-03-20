import Vision.GUI
import Util.Geometry
import Vision
import Util.Camera
import Numeric.LinearAlgebra

c = syntheticCamera $ easyCamera (pi/3) (5,5,5) (1,0,10) 0

ic = infoCam (unsafeFromMatrix c)

ipts = toImagePlane ic 1 [Point 1.2 0, Point 0 1.2]

main = do
    putStr. dispf 2 $ c
    runIt $ browser3D "synth camera" [drw] (const Draw)

drw = Draw [axes3D 4
           , color red [ showCamera 1 ic Nothing
                       , (text3DAtF Helvetica12 (inhomog $ ipts!!0) "x")
                       , (text3DAtF Helvetica12 (inhomog $ ipts!!1) "y")
                       ]
           ]

