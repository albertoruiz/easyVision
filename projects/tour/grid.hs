import EasyVision
import Util.Misc(splitEvery)
import Data.List(tails)

grid n = map (blockImage . splitEvery n . take (n*n)) . tails

main = run  $    camera ~> resize (mpSize 4) . rgb
            ~~>  grid 6
            >>=  observe "grid" id >>= timeMonitor

