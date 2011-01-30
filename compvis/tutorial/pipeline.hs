-- time ./pipeline 'video.avi -benchmark -loop 1 -frames 100' +RTS -N2
-- time ./pipeline 'video.avi -benchmark -loop 1 -frames 100' '--levels=(1,20)' +RTS -N2
import EasyVision
import Util.Options

compose = foldr (.) id

expensive k = compose (replicate k f) where
    f im = resize (size im) . block . gaussS 10 $ im
    block im = blockImage [[im,im],[im,im]]

balance f = compose . map (pipeline . f)

main = do
    s <- getOption "--stages" =<< uncurry replicate `fmap` getOption "--levels" (20,1)
    putStrLn $ "stages = " ++ show s

    run $   camera ~> float . gray
        >>= observe "original" id
        ~~> balance expensive s
        >>= observe "result" id
