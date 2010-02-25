import EasyVision

camera = findSize >>= getCam 0 ~> channels
--observe winname f = monitor winname (mpSize 20) (drawImage'.f)
run c = prepare >> (c >>= launch . (>> return ()))

main = do
    corners <- getCornerDetector

    run $ camera ~> float . gray >>= corners >>= cornerMonitor "corners"
