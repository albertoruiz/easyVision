import EasyVision

camera = findSize >>= getCam 0 ~> channels
run c = prepare >> (c >>= launch . (>> return ()))

main = do
    corners <- getCornerDetector
    mintrk <- getOption "--mintrk" 20
    run $ camera ~> float . gray >>= corners >>= cornerTracker mintrk
