import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Control.Arrow
import Control.Monad
import Vision
import Numeric.LinearAlgebra hiding ((.*))

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage'.f)
run c = prepare >> (c >>= launch . (>> return ()))

main = run $ camera .&. userParam
      ---     >>= observe "Video" (rgb.fst)
           ~>  g &&& snd
           >>= monitor "Channel S" (mpSize 20) (drawcont.fst)
           ~> rectify *** id
           >>= observe "Whiteboard" fst
           ~> proc
           >>= observe "proc" id


g (img,p) = (img,cs2) where
    cs2 = map (redu.fst3) $ contours 100 pixarea (fromIntegral $ thres p) False (sCh img)
    pixarea = h*w*50`div`1000
    (Size h w) = size (gray img)
    redu = take 4 . douglasPeuckerClosed (peuck p)
    fst3 (a,_,_) = a

drawcont (img,cs2) = do
    drawImage' (sCh img)
    pixelCoordinates (size (gray img))
    mapM_ shcont cs2

shcont c = do 
    setColor 1 0 0
    lineWidth $= 2
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 4
    setColor 1 1 1
    renderPrimitive Points $ mapM_ vertex c

(.&.) = liftM2 (liftM2 (,))

data Param = Param { peuck :: Double,
                     thres :: Int,
                     thres2 :: Int,
                     sigma :: Float,
                     sc    :: Float }

userParam = do
    o <- createParameters [("peuck",realParam 10 0 20),
                           ("sigma",realParam 0 0 10),
                           ("thres",intParam  50 0 255),
                           ("thres2",intParam  128 0 255),
                           ("scale",realParam  1 0 10)]
    return $ return Param `ap` getParam o "peuck"
                          `ap` getParam o "thres"
                          `ap` getParam o "thres2"
                          `ap` getParam o "sigma"
                          `ap` getParam o "scale"

rectify (img,pixs) = warp 0 (mpSize 20) h' (gray img)
    where h = estimateHomography a4aux (map pl pts)
          pts = pixelsToPoints (mpSize 20) (head pixs)
          a4aux = [[-1,-r],[1,-r],[1,r],[-1,r]]
          r = 1/ratio
          pl (Point x y) = [x,y]
          ratio = sqrt 2
          h' = if null pixs then ident 3 else h

proc (x,p) = ((sc p) .*) $ gaussS (sigma p) $ float $ binarize8u (fromIntegral $ thres2 p) z
    where z = toGray $ gaussS (sigma p) (float x)