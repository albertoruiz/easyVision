import EasyVision

camera = prepare >> findSize >>= getCam 0 ~> rgb . channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run = launch . (>> return ())

grid n = map (blockImage . partit n) . partit (n*n) . map (resize (mpSize 4))
    where partit _ [] = []
          partit k l = take k l : partit k (drop k l)

main = camera ~~> grid 5 >>= observe "grid" id >>= run
