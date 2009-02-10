import EasyVision
import Tutorial(run, camera, observe, save)

grid n = map (blockImage . partit n) . partit (n*n)
    where partit _ [] = []
          partit k l = take k l : partit k (drop k l)

main = run $   camera
           ~~> grid 5 . map rgb
           >>= observe "grid" id
           >>= save toYUV
