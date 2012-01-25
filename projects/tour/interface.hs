import Vision.GUI
import ImagProc

main = run clickPoints

clickPoints :: ITrans Channels ([Point], ImageGray)
clickPoints = transUI $ interface (Size 240 320) "pts" [] ft updt [] r sh
  where
    r _ pts input = (pts, (pts, notI . grayscale $ input))
    sh _ _ (pts,x) = Draw [ Draw x, color green, pointSz 3, points pts ]
    updt = [(key (MouseButton LeftButton), \_ p st -> p:st)]
    ft _ _ = return ()

