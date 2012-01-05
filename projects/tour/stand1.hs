import EasyVision
import Graphics.UI.GLUT.Callbacks

main = runIt win

win = standalone (Size 100 400) "click to change" x0 acts [] sh
  where
    x0 = 7
    sh = text (Point 0 0) . show
    acts = [((MouseButton LeftButton, Down, modif), \roi pt -> (+1)) ]

