import EasyVision

main = do
    corners <- getCornerDetector
    mintrk <- getOption "--mintrk" 20
    run $ camera ~> float . gray >>= corners >>= cornerTracker mintrk >>= timeMonitor
