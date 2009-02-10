import EasyVision
import Tutorial(run,camera,observe)

main = run $   camera
           >>= observe "original" rgb
           ~>  highPass8u Mask5x5 . median Mask5x5 . gray
           >>= observe "high-pass filter" id
