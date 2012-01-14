import EasyVision.GUI  (observe, run, camera)
import ImagProc        (rgb)

main = run (observe "image" rgb)
