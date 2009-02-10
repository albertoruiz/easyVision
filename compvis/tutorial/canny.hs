import EasyVision
import Tutorial(run, camera, observe)

edges = canny (0.1,0.3) . gradients . gaussS 2 . float . gray

main = run $   camera
           >>= observe "Canny's operator" (notI . edges)
