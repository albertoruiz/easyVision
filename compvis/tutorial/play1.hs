import EasyVision

camera = findSize >>= getCam 0

observe = monitor "Video" (mpSize 20) drawImage

run c = prepare >> (c >>= launch . (>> return ()))


main = run (camera >>= observe)
