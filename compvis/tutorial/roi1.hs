import EasyVision

main = run $ camera ~> grayscale
           >>= selectROI "ROI" id
           >>= observe "invert" (notI . uncurry (flip setROI))


