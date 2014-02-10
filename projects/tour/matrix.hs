import Vision.GUI
import Image.Processing
import Image.Capture
import Numeric.LinearAlgebra

main = do
    img <- loadRGB "../../data/images/pano/pano002.jpg"
    runIt $ browser "image" [Draw img, Draw (f img)] (const id)

f = mat2img . g . img2mat . toFloat . grayscale . channelsFromRGB

g = {-subMatrix (100,100) (200,200) . -} trans

