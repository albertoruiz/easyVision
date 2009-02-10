import EasyVision
import Graphics.UI.GLUT
import Control.Monad(when)
import Tutorial(run, camera)

main = run (camera ~> gray >>= bgDiff)

bgDiff cam = do
    w <- evWindow (True,undefined) "bg diff" (mpSize 20) Nothing (mouse kbdQuit)
    return $ do
        img <- cam
        (rec,_) <- getW w
        when rec (putW w (False, img))
        (_,bg) <- getW w
        let r = absDiff8u img bg
        inWin w $ drawImage r
        return r
  where
    mouse _ st (Char 's') Down _ _ = st $= (True,undefined)
    mouse def _ a b c d = def a b c d
