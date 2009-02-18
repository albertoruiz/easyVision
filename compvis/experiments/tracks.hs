import EasyVision

camera = findSize >>= getCam 0 ~> channels
run c = prepare >> (c >>= launch . (>> return ()))

main = do
    cd <- getCornerDetector
    run $ camera ~> float . gray >>= cd >>= cornerTracker
