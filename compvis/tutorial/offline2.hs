{- ./offline 'sourcevideo -benchmark -loop 1' --save=/dev/stdout --size=10
   | mencoder - -o result.avi -ovc lavc -fps 15 -}

import EasyVision

process f = do
    outfile <- optionString "--save" "saved.yuv"
    xs <- readFrames 0
    let ys = map toYUV . f . map channels $ xs
    writeFrames outfile ys

g = notI . canny (0.1,0.3) . gradients . gaussS 2 . float . resize (mpSize 10) . gray

grid n = map (blockImage . partit n) . partit (n*n)
    where partit _ [] = []
          partit k l = take k l : partit k (drop k l)

main = process (grid 2 . map g)
