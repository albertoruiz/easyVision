import EasyVision
import Data.Colour.Names
import Graphics.UI.GLUT.Callbacks.Window as GL

main = run $ camera >>= observe "Cosa" (notI . grayscale) >>= sMonitorR "result" f >>= timeMonitor

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

--------------------------------------------------------------------------------

sMonitorR name f = interface (mpSize 10) name 0 (const.const.const) acts (const.const.const) sv Nothing (const (,)) g
  where
    g roi k x = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [((MouseButton WheelUp,   Down, modif), (+1))
           ,((SpecialKey  KeyUp,     Down, modif), (+1))
           ,((MouseButton WheelDown, Down, modif), pred)
           ,((SpecialKey  KeyDown,   Down, modif), pred)]
    sv =   [((SpecialKey  KeyF3,     Down, modif), print)]

