-- experiments on multiview calibration from stabilized pose

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set)

main = do
    sz <- findSize
    prepare

    cam <- getCam 0 sz >>= withChannels >>= findPolygons Nothing asym

    w <- evWindow () "multipose" sz Nothing (const $ kbdQuit)

    launch $ do
        inWin w $ do
            (img,rects) <- cam
            drawImage (gray img)
            pointCoordinates (size (gray img))

            setColor 1 0 0
            lineWidth $= 3
            mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) (map fst rects)
            text2D 0.9 0.6 (show (length rects))



asym = [ [ 0, 0]
       , [ 0, 9.7]
       , [ 5, 9.7]
       , [ 5, 14.9]
       , [ 10.5, 14.9]
       , [ 10.5, 0] ]

asym' = [ [ 0, 0]
       , [ 0, 9.7]
       , [ 5, 14.9]
       , [ 10.5, 14.9]
       , [ 10.5, 0] ]

a4 = [[   0,            0]
     ,[   0, (2.10*ratio)]
     ,[2.10, (2.10*ratio)]
     ,[2.10,           0]] where ratio = sqrt 2

