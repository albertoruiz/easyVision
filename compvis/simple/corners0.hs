import EasyVision

main = do
    corners <- getCornerDetector

    run $ camera ~> float . gray >>= corners >>= cornerMonitor "corners"
