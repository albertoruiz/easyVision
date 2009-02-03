{- ./offline 'sourcevideo -benchmark -loop 1' --save=/dev/stdout
   | mencoder - -o result.avi -ovc lavc -fps 25 -}

import EasyVision
import Data.List(foldl1',tails)

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

g = canny (0.1,0.3) . gradients . gaussS 2 . float . gray

h k = map (notI . foldl1' orI . reverse . take k) . tail . tails
main = run $   camera
           >>= observe "original" rgb
           ~~> h 5 . map g
           >>= observe "result" id
           >>= save toYUV
