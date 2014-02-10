-- threaded GUI, output discarded
-- clean exit by user ESC or end of stream

import Vision.GUI.Simple
import Image

main = run (observe "image" rgb)
