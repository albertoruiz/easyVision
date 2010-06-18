import EasyVision
import Graphics.UI.GLUT hiding (Size,Point)
import Vision
import OpenCV
import Control.Monad(when)

main' = run (camera ~> gray >>= face' >>= monitor "Face" (mpSize 20) f >>= timeMonitor)
    where f (im,r) = do
               drawImage im
               mapM_ drawROI r

main = run (camera ~> gray >>= face >>= observe "Face" id >>= virtual >>= timeMonitor)

pini = syntheticCamera $ easyCamera (60*degree) (0.5,0.5,0.5) (0.5,0.5,5)  0

virtual cam = do
    v <- evWindow3D pini "kk" 500 (const kbdQuit)
    return $ do
        x <- cam
        let p = roiCenter (theROI x)
            [Point xc yc] = pixelsToPoints (size x) [p]
            d = 7 -- - fromIntegral (r2-r1) / 100
        when (r1 (theROI x) > 0) $ do
            putW v $ syntheticCamera $ easyCamera (30*degree) (sqrt (d^2 - xc^ 2 -yc^ 2),3*xc,3*yc) (0,0,0)  0

        inWin v $ do
            p <- getW v
            cameraView p 1 0.1 100
            
            translate (Vector3 (-0.5) (-0.5) (-0.5::Float))
            scale 4 0.5 (0.5::Float)
            --setColor 1 1 1
            --renderPrimitive LineStrip $ mapM_ vertex cube
--            scale 0.5 0.5 (1::Float)
--            translate (Vector3 (-5) 0 (2::Float))
--            renderPrimitive LineStrip $ mapM_ vertex cube
            --houseModel
            setColor 1 0 0; side 0
            setColor 0 1 0; side 1
            setColor 0 0 1; side (1/2)
            setColor 0 1 1; side (-1/2)
            setColor 1 1 0; side (-1)

            return ()
        return x

cube :: [[Double]]
cube = [[0,0,0],
        [1,0,0],
        [1,1,0],
        [0,1,0],
        [0,0,0],
        [0,0,1],
        [1,0,1],
        [1,1,1],
        [0,1,1],
        [0,0,1],
        [0,1,1],
        [0,1,0],
        [1,1,0],
        [1,1,1],
        [1,0,1],
        [1,0,0]]

side x = renderPrimitive Polygon $ mapM_ vertex $ 
                [[x,0,0::Double],
                 [x,1,0],
                 [x,1,1],
                 [x,0,1],
                 [x,0,0]]

face cam = do
    detect <- cascadeClassifier "/usr/local/share/opencv/haarcascades/haarcascade_frontalface_alt.xml"
    return $ do
         x <- cam
         let r = detect x 1
         if null r then return x
                   else return $ modifyROI (const $ head r) x


face' cam = do
    detect <- cascadeClassifier "/usr/local/share/opencv/haarcascades/haarcascade_frontalface_alt.xml"
    nested <- cascadeClassifier "/usr/local/share/opencv/haarcascades/haarcascade_eye_tree_eyeglasses.xml"
    return $ do
         x <- cam
         let r = detect x 1
         let r2 = if null r then [] else nested (modifyROI (const $ head r) x) 2
         return (x,r++r2)
