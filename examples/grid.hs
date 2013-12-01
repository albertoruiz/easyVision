import Vision.GUI
import Image.Processing
import Util.Misc(splitEvery)
import Data.List(tails)

grid n = map (blockImage . splitEvery n . take (n*n)) . tails

main = run  $    observe "source" id
            >>>  arr (resize (Size 96 120) . rgb)
            >>>  arrL (grid 5)
            >>>  observe "grid" id

