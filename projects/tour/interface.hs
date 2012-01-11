import EasyVision.GUI
import ImagProc
import Graphics.UI.GLUT.Callbacks

main = run camera clickPoints

clickPoints :: Trans Channels ([Point], ImageGray)
clickPoints = transUI $ interface (Size 240 320) "pts" [] ft acts [] r sh
  where
    r _ pts input = (pts, (pts, notI . grayscale $ input))
    sh _ _ (pts,x) = Draw [ Draw x, color green, pointSz 3, points pts ]
    acts = [((MouseButton LeftButton, Down, modif), \_ p st -> p:st)]
    ft _ _ = return ()

