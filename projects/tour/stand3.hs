import EasyVision
import Graphics.UI.GLUT.Callbacks
import Util.Misc(replaceAt)

main = runIt win

win = editor update save "editor" [2,4 .. 10] sh
  where
    sh k x = Draw  [  color white, text (Point 0 0) (show x)
                   ,  color yellow, text (Point 0.9 0.8) ("# "++show k) ]
    update =  [  op (Char '+') succ
              ,  op (Char '-') pred ]
    save = [(ctrlS, \_roi _pt (_k,xs) -> print xs)]
    ctrlS = (Char '\DC3', Down, modif {ctrl=Down})
    op c f = updateItem (c,Down,modif) (const.const $ f)