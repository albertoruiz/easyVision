import EasyVision

grid n = map (blockImage . splitEvery n) . splitEvery (n*n) . map (resize (mpSize 4))
    where splitEvery _ [] = []
          splitEvery k l = take k l : splitEvery k (drop k l)

main = run $   camera ~> rgb
           >>= observe "original" id
           ~~> grid 2
           >>= observe "first grid" id
           ~~> grid 3
           >>= observe "second grid" id
