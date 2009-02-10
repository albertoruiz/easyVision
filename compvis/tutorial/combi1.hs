import EasyVision
import Tutorial(camera, observe, run, grid)

main = run $   camera ~> rgb
           >>= observe "original" id
           ~~> grid 2
           >>= observe "first grid" id
           ~~> grid 3
           >>= observe "second grid" id
