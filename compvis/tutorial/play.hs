import EasyVision
import Tutorial(run, camera, observe)

main = run $ camera >>= observe "Video" rgb
