import EasyVision

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

main = run $   camera
           >>= observe "original" rgb
           ~>  highPass8u Mask5x5 . median Mask5x5 . gray
           >>= observe "filter" id
