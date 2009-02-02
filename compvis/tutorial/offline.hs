{- ./offline 'sourcevideo -benchmark -loop 1' --save=/dev/stdout
   | mencoder - -o result.avi -ovc lavc -fps 25 -}

import EasyVision

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

save f cam = do
    filename <- optionString "--save" "saved.yuv"
    sz <- findSize
    sv <- openYUV4Mpeg sz filename Nothing
    return $ do
        x <- cam
        sv (f x)
        return x

g = notI . canny (0.1,0.3) . gradients . gaussS 2 . float . gray

grid n = map (blockImage . partit n) . partit (n*n) . map (resize (mpSize 10))
    where partit _ [] = []
          partit k l = take k l : partit k (drop k l)

main = run $   camera
           >>= observe "original" rgb
           ~~> grid 2 . map g
           >>= observe "result" id
           >>= save toYUV
