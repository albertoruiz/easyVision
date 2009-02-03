-- time ./pipeline 'video.avi -benchmark -loop 1 -frames 100' +RTS -N2
-- time ./pipeline 'video.avi -benchmark -loop 1 -frames 100' '--levels=(1,20)' +RTS -N2

import EasyVision

camera = findSize >>= getCam 0 ~> float . gray . channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

compose = foldr (.) id

expensive k = compose (replicate k f) where
    f im = resize (size im) . block . gaussS 10 $ im
    block im = blockImage [[im,im],[im,im]]

balance f = compose . map (pipeline . f)

main = do
    s <- getOption "--stages" =<< uncurry replicate `fmap` getOption "--levels" (20,1)
    putStrLn $ "stages = " ++ show s

    run $   camera
        >>= observe "original" id
        ~~> balance expensive s
        >>= observe "result" id
