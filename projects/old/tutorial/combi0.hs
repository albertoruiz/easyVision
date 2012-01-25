import EasyVision

grid n = map (blockImage . splitEvery n) . splitEvery (n*n) . map (resize (mpSize 4))
    where splitEvery _ [] = []
          splitEvery k l = take k l : splitEvery k (drop k l)

main = run  $    camera ~> rgb
            ~~>  grid 5 
            >>=  observe "grid" id
