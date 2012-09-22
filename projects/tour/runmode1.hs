-- threaded GUI, output discarded
-- clean exit by user ESC or end of stream

import Vision.GUI
import ImagProc

main = run (observe "image" rgb)
