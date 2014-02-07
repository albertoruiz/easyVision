import Vision.GUI
import Image.Processing

main = run $ observe "Mirror" (mirror.grayscale)

mirror im = blockImage [[im1, mirror8u 1 im1]] where
    Size h w = size im
    ROI r1 r2 c1 c2 = (roi im)
    im1 = resize (Size h (div w 2)) (modifyROI (const $ ROI r1 r2 c1 (div w 2)) im)

