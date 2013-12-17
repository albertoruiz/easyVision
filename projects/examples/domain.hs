import Vision.GUI
import Image.Processing
import Image.Capture
import Image.ROI
import Util.Homogeneous
import Numeric.LinearAlgebra

basis = imageBasis vga

copyROI b x = copy b [(x,topLeft (roi x))]

vga = Size 480 640

k v = constantImage v vga :: Image Float

u = k 0.2
up = modifyROI (roiArray 3 3 2 2) (u |*| xIb basis) -- u
x = k 0 -- `copyROI` up
y = k 0 `copyROI` up
t a = domainTrans32f (k 0.5) (x,y) a

main = do
    base <- toFloat . rgbToGray . resize vga <$> loadRGB "../../data/images/transi/dscn2070.jpg"
    runIt $ browser "domain transformation" [t base, base] (const Draw)

