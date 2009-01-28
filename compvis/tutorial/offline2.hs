{- ./offline 'sourcevideo -benchmark -loop 1' --save=/dev/stdout
   | mencoder - -o result.avi -ovc lavc -fps 15 -}

import EasyVision

process f = do
    filename <- fmap (maybe "saved.yuv" id) (getRawOption "--save")
    findSize >>= readFrames 0 >>= writeFrames filename . f

g = toYUV . notI . resize (mpSize 10) . gray . channels

main = process (map g)
