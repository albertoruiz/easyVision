import EasyVision.GUI
import ImagProc
import Util.Misc(splitEvery)
import Data.List(tails)

grid n = map (blockImage . splitEvery n . take (n*n)) . tails

main = run  $    arr (resize (Size 96 120) . rgb)
            >>>  arrL (grid 5)
            >>>  observe "grid" id

