import EasyVision.GUI
import ImagProc

main = run camera $ observe "Mirror" (mirror.grayscale)

mirror im = blockImage [[im1, mirror8u 1 im1]] where
    Size h w = size im
    roi = (theROI im) {c2 = div w 2}
    im1 = resize (Size h (div w 2)) (modifyROI (const roi) im)

