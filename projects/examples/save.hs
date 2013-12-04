import Vision.GUI.Simple
import Image.Processing
import Image
import Image.Capture
import Image.Devel(gray2rgb)

main = run $ arr (resize (Size 200 200)) >>> observe "source" id

main' = do
    cam <- camera
    cam >> cam >> cam
    Just img <- cam
    saveImage "kk.ppm" (resize (Size 200 200) img)

