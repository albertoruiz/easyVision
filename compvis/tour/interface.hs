{-# LANGUAGE NoMonomorphismRestriction #-}

import EasyVision
import ImagProc.Ipp.Core(fullroi)
import Graphics.UI.GLUT.Callbacks.Window as GL
import qualified Graphics.UI.GLUT as G
import Control.Arrow

main = run $ camera >>= observe "Cosa" (notI . grayscale) >>= sMonitorR1 "result1" f >>= sMonitorR2 "result2" f >>= monPts >>=  timeMonitor

f roi x = [ msg "grayscale"        [ Draw g, color red, lineWd 2, Draw square]
          , msg "gaussian filter " [ Draw smooth ]
          , msg "canny edges"      [ Draw (notI edges) ]
          , msg "rgb"              [ color red, Draw square ]
          , nothing ]
  where
    img = rgb x 
    g   = setRegion roi (grayscale x)
    f   = float g
    smooth = gauss Mask5x5 f
    th = 0.5
    sigma = 2
    edges = canny (th/3,th) . gradients $ smooth

    msg s x = Draw [ Draw img, Draw x , color yellow, text (Point 0.9 0.65) s ]
    nothing = Draw [color yellow, text (Point 0.9 0.65) "nothing" ]
    square = Closed [Point 0 0, Point 0.2 0, Point 0.2 0.2, Point 0 0.2]

--------------------------------------------------------------------------------

sMonitorR' name myft f = interface (mpSize 10) name 0 myft (c3 acts) (c3 sv) nothingR (const (,)) g
  where
    g roi k x = r !! j
      where
        r = f roi x
        j = k `mod` length r
    acts = [((MouseButton WheelUp,   Down, modif), (+1))
           ,((SpecialKey  KeyUp,     Down, modif), (+1))
           ,((MouseButton WheelDown, Down, modif), pred)
           ,((SpecialKey  KeyDown,   Down, modif), pred)]
    sv =   [((SpecialKey KeyF12,              Down, modif), print)]

sMonitorR name f = sMonitorR' name (\_ _ ->  return ()) f
sMonitorR1 name f = sMonitorR' name ft1 f
sMonitorR2 name f = sMonitorR' name ft2 f


ft1 w x = evROI w G.$= shrink (50,50) (theROI (grayscale x))
ft2 w x = ft1 w x >> evPrefSize w G.$= Just (mpSize 25)

c3 = map (id *** const.const)

--------------------------------------------------------------------------------

monPts = interface (mpSize 10) "pts" [] (\_ _ -> return ()) acts [] nothingR (const (,)) g
  where
    g _ pts x = Draw [ Draw (grayscale x), color green, pointSz 3, Draw (Points pts) ]
    acts = [((MouseButton LeftButton, Down, modif), \_ p st -> p:st)]

newtype Points = Points [Point]
instance Renderable Points where
  render (Points ps) = G.renderPrimitive G.Points (mapM_ G.vertex ps)

