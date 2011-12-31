import OpenCV
import EasyVision
import Graphics.UI.GLUT
import Control.Arrow

main = run $ camera ~> grayscale
           >>= mserRaw .@. winMSERParams
           ~> (fst &&& mser.snd) >>= monitor "MSER" (mpSize 20) sh >>= timeMonitor

sh (im, cs) = do
    drawImage im
    pointCoordinates (size im)
    setColor 1 0 0
    text2D 0.9 0.6 $ show (length cs)
    mapM_ shcont cs

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
shcont (Open c) = do
    renderPrimitive LineStrip $ mapM_ vertex c

mser im = map fst3 . contours 100 50 128 False $ im
  where
    fst3 (a,_,_) = Closed $ pixelsToPoints (size im) a -- $ douglasPeucker 1 a

