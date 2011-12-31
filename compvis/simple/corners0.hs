import EasyVision

main = do
    corners <- getCornerDetector

    run $ camera ~> float . grayscale >>= corners >>= cornerMonitor "corners"
