import EasyVision

camera = findSize >>= getCam 0 ~> float . gray . channels
observe winname = monitor winname (mpSize 20) drawImage
run c = prepare >> (c >>= launch . (>> return ()))

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)

interpolate (a:b:xs) = a: (0.5.*a |+| 0.5.*b) :interpolate (b:xs)

main = run   $ camera
           ~~> drift 0.9
           >>= observe "drift"
           ~~> interpolate
           >>= zoomWindow "zoom" 600 toGray
