import Vision.GUI
import ImagProc
import Util.Misc(replaceAt)

main = runIt win

win = editor update save "editor" [2,4 .. 10] sh
  where
    sh k x = Draw  [  color white, text (Point 0 0) (show x)
                   ,  color yellow, text (Point 0.9 0.8) ("# "++show k) ]
    update =  [  op (Char '+') succ
              ,  op (Char '-') pred ]
    save = [(ctrlS, \_roi _pt (_k,xs) -> print xs)]
    ctrlS = kCtrl (key (Char '\DC3'))
    op c f = updateItem (key c) (const.const $ f)

