import Vision.GUI.Simple ( Drawing(Draw), browser, runIt )
import Image.Capture
import Image.ROI
import Image.Processing
import Numeric.LinearAlgebra
import Util.Homogeneous
import Util.Rotation


main = do
    ims <- readImages [ "../../data/images/calibration/cube1.png"
                      , "../../data/images/calibration/cube3.png"]
    let im1 = modifyROI (shrink (20,10)) (ims!!0)
        im2 = modifyROI (roiArray 2 2 2 2) (ims!!1)
        im3 = copy im1 [ (im2,Pixel (300) 400)
                       , (im2,Pixel (-50) (-50))
                       , (im2,Pixel (-1000) 1000)
                       , (resize (Size 120 160) im1, Pixel 100 100)
                       ]
        im4 = set im3 [ (roiArray 3 3 2 2 (roi im3), Word24 0 128 0)
                      , (ROI 400 500 600 700, Word24 255 255 0)
                      , (ROI (-100) 120 125 200, Word24 255 128 0)
                      ]
        small = setROI (ROI 100 200 50 50) (ims!!0)
        empty = setROI (ROI 2 1 0 0) im1
        im5 = resize (Size 3 4) im1
        im6 = resize (Size (-5) 100) small
        im7 = resize (Size 120 160) small
        im8 = resize (Size 120 160) empty
        im9 = resizeFull (Size 120 160) $ modifyROI (roiArray 2 2 2 2) (ims!!0)
        im10 = warpon (ims!!0) [(h1, ims!!1)]
        im11 = warpon im1 [(h1,im2),(h2,im2), (h3,im2)]
        im12 = warp (Word24 128 0 128) (Size 600 400) h1 (ims!!0)
    runIt $ browser "tests" [im12,im11,im10,im9,im7,im5,empty,small,im4,im3,im1,im2] (const Draw)

h1 = desp (0.5, 0.7) <> rot3 (0.3) <> scaling 0.5
h2 = desp (-0.5,0) <> h1
h3 = (3><3)[1,0,0,
            0,1,0,
            5,0,3]

