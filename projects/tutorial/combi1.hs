import EasyVision
import Util.Misc(splitEvery)

grid n = map (blockImage . splitEvery n) . splitEvery (n*n) . map (resize (mpSize 4))
    
main = run  $    camera ~> rgb
            >>=  observe "original" id
            ~~>  grid 2
            >>=  observe "first grid" id
            ~~>  grid 3
            >>=  observe "second grid" id

