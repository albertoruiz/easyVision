import EasyVision

camera = findSize >>= getCam 0 ~> rgb . channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

grid n = map (blockImage . partit n) . partit (n*n) . map (resize (mpSize 4))
    where partit _ [] = []
          partit k l = take k l : partit k (drop k l)

main = run $ camera ~~> grid 5 >>= observe "grid" id
