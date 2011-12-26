import EasyVision
import Data.Colour.Names

main = run $ camera >>= sMonitor "result" f >>= timeMonitor

f roi x = [ msg "grayscale"        [ Draw g]
          , msg "gaussian filter " [ Draw smooth ]
          , msg "canny edges"      [ Draw (notI edges) ]
          , nothing ]
  where
    img = rgb x 
    g   = setROI roi (grayscale x)
    f   = float g
    smooth = gauss Mask5x5 f
    th = 0.5
    sigma = 2
    edges = canny (th/3,th) . gradients $ smooth

    msg s x = Draw [ Draw img, Draw x , color yellow, text (Point 0.9 0.65) s ]
    nothing = Draw [color yellow, text (Point 0.9 0.65) "nothing" ]

