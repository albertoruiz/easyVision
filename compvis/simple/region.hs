import EasyVision hiding (run)
import Graphics.UI.GLUT hiding (Point,Size)
import Data.Colour.Names as Col hiding (gray)
import System.Process(system)

----------------------------------------------------------------------

run c = prepare >> (c >>= launchFreq 10 . (>> return ()))

main = run $ camera
           >>= regionMarker (mpSize 20) rgb 
           >>= rectifyRegion rgb 400 .@. winAspectRatioParam ~> snd.snd
           >>= observe "Rectified" id

----------------------------------------------------------------------

