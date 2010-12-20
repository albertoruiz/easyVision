import EasyVision as EV
import Graphics.UI.GLUT
import Util.Misc(diagl)
import Util.Rotation(rot3)
import Control.Arrow
import Data.Colour.Names as Col
import Vision(desp)
import Numeric.LinearAlgebra((<>))

main = run $ camera ~> EV.gray >>= wcontours ~> (id *** contSel) >>= monitorC

monitorC = monitor "contours" (mpSize 20) sh where
    sh (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' orange
        lineWidth $= 1
        mapM_ shcont cs
        let wcs1 = map (head . f) cs
            wcs2 = map (last . f) cs
        setColor' red
        lineWidth $= 2
        mapM_ shcont wcs1
        setColor' blue
        lineWidth $= 2
        mapM_ shcont wcs2
    shcont (Closed c) = do
        renderPrimitive LineLoop $ mapM_ vertex c
    shcont (Open c) = do
        renderPrimitive LineStrip $ mapM_ vertex c


icaConts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

f c@(Closed ps) = map (transPol t) . icaConts . whitenContour $ c
  where
    t = desp (ox,oy) <> diagl [0.05,0.05,1]
    (ox,oy,_,_,_) = momentsContour ps



