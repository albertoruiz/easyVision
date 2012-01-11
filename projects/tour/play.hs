import EasyVision.GUI  (observe, run, camera)
import ImagProc        (rgb)

main = run camera (observe "image" rgb)
