import EasyVision

camera = prepare >> findSize >>= getCam 0 ~> float . gray . channels
observe winname = monitor winname (mpSize 20) drawImage
run = launch . (>> return ())

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)

interpolate (a:b:xs) = a: (0.5.*a |+| 0.5.*b) :interpolate (b:xs)

main = camera ~~> drift 0.9
              >>= observe "drift"
              ~~> interpolate
              >>= zoomWindow "zoom" 600 toGray
              >>= run
