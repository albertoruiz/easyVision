import EasyVision

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run = launch . (>> return ())

main = do
    prepare
    camera >>= observe "original" rgb
           ~>  highPass8u Mask5x5 . median Mask5x5 . gray
           >>= observe "filter" id
           >>= run
