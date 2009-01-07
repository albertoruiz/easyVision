import EasyVision

camera = prepare >> findSize >>= getCam 0

observe winname f = monitor winname (mpSize 20) (drawImage.f)

run = launch . (>> return ())


main = camera >>= observe "Video" id >>= run
