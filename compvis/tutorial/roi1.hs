import EasyVision

main = run $ camera ~> grayscale
           >>= selectROI "ROI" id
           >>= observe "invert" (notI . setROI)

setROI (img,roi) = modifyROI (const roi) img

