import Vision.GUI.Simple
import Image
import Image.Capture

main = do
    img <- loadRGB "../../data/images/transi/dscn2070.jpg"
    runIt $ browser "image" [img] (const Draw)

