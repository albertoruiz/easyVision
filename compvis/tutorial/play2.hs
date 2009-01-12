import EasyVision

camera = findSize >>= getCam 0
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

f = gaussS 5.7 . float . gray . channels

main = run (camera >>= observe "Gauss" f)
