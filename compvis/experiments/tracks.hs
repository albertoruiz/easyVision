import EasyVision
import Data.List
import Data.Function
import Control.Arrow
import Vision
import Numeric.LinearAlgebra hiding ((.*))

camera = findSize >>= getCam 0 ~> channels
run c = prepare >> (c >>= launch . (>> return ()))
f .***. g = uncurry zip . (f *** g) . unzip

main = do
    corners <- getCornerDetector
    sc <- getOption "--scale" 0.5
    alpha <- getOption "--alpha" 0.95

    run $   camera ~> float . gray ~~> drop 3
        >>= zoomWindow "zoom" 600 toGray
        >>= corners >>= cornerTracker -- >>= board >>= cornerMonitor "best"
--        ~>  id *** infoTracks
        ~~> (id .***. zip [1..])
--        ~~> filter (okFrame . snd)
        >>= align sc
        >>= drift alpha
        >>= monitor "selected" (mpSize 5) (drawImage')

--------------------------------------------------------------------------

align sc cam = return $ do
    (img,(k,tracks)) <- cam
    let okt = filter ((==k).length) tracks
        as = map head okt
        bs = map last okt
        h = estimateHomographyRaw (map pl bs) (map pl as)
    return $ warp 1 (Size 600 600) (scaling sc <> h) img

pl (Point x y) = [x,y]

drift alpha = virtualCamera drifter
    where drifter (a:b:rest) = a : drifter ((alpha .* a |+| (1-alpha).* b):rest)

--------------------------------------------------------------------------

board cam = return $ do
    (img, tracks) <- cam
    print (infoTracks tracks)
    return (img, pointsToPixels (size img) $ map head $ filter ((>25).length) $ tracks)

infoTracks = reverse . ac . reverse . map v . group . sort . map length

v xs = (length xs, head xs)

ac l = zip (scanl1 (+) c) m where (c,m) = unzip l

okFrame xs = g . (\(k,x)->(k, head x, last x)) $ debug xs
    where g (k,(a,_),(b,_)) = True

