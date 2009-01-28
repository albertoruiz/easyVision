{- ./offline 'sourcevideo -benchmark -loop 1' --save=/dev/stdout
   | mencoder - -o result.avi -ovc lavc -fps 15 -}

import EasyVision

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

save f cam = do
    filename <- fmap (maybe "saved.yuv" id) (getRawOption "--save")
    sz <- findSize
    sv <- openYUV4Mpeg sz filename Nothing
    return $ do
        x <- cam
        sv (f x)
        return x

main = run $   camera
           >>= observe "original" rgb
           ~>  notI . gray
           >>= observe "filter" id
           >>= save toYUV
