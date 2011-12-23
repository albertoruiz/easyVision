import EasyVision
import Data.List
import Data.Function
import Control.Arrow
import Vision
import Numeric.LinearAlgebra
import qualified Data.Map as M
import Util.Options

f .***. g = uncurry zip . (f *** g) . unzip

main = do
    corners <- getCornerDetector
    sc <- getOption "--scale" 0.5
    alpha <- getOption "--alpha" 0.95

    mintrk <- getOption "--mintrk" 20

    run $   camera ~> float . gray ~~> drop 3
--        >>= zoomWindow "zoom" 600 toGray
        >>= corners >>= cornerTracker mintrk -- >>= board >>= cornerMonitor "best"

--        ~~> (id .***. (map nub. inits))    -- base frames

        ~~> (id .***. (tail . scanl (\m (k,b,h) -> M.insert k (m M.! b <> h) m) (M.fromList [(0,ident 3)])))  -- rectification chain

        ~~> zip [0..] ~> (\(k,((im,_),m)) -> (im, m M.! k))

        ~~> warpAll sc
        >>= zoomWindow "zoom" 1200 toGray

--        ~~> (id .***. (tail . scanl (\m x -> M.insertWith (+) (fst x) 1 m) M.empty))  -- number of frames linked to each base frame
--        >>= monitor "selected" (mpSize 5) auxsh

--        ~~> filter (okFrame . snd)



--------------------------------------------------------------------------

warpOne sc (im,h) = warp 1 (Size 1200 1200) (scaling sc <> h) im

warpAll sc xs = scanl goWarp im0 (tail xs) where
    im0 = warpOne sc (head xs)
    goWarp prev (im,h) = warpOn prev (scaling sc <> h) im


align sc cam = return $ do
    (img,(k,tracks)) <- cam
    let okt = filter ((==k).length) tracks
        as = map head okt
        bs = map last okt
        h = estimateHomographyRaw (map pl bs) (map pl as)
    return $ hp $ warp 1 (Size 1200 1200) (scaling sc <> h) img

pl (Point x y) = [x,y]

drift alpha = virtualCamera drifter
    where drifter (a:b:rest) = a : drifter ((alpha .* a |+| (1-alpha).* b):rest)

--------------------------------------------------------------------------

hp img = float $ scale32f8u a b img where
    (a,b) = EasyVision.minmax img


board cam = return $ do
    (img, tracks,landmarks,_) <- cam
--    print (infoTracks tracks)
    return (img, pointsToPixels (size img) $ map head $ filter ((>25).length) $ tracks)


okFrame xs = g . (\(k,x)->(k, head x, last x)) $ xs
    where g (k,(a,_),(b,_)) = True

auxsh ((im,tks),k) = do
    drawImage' im
    text2D 20 20 $ show $ (length $ M.keys k, reverse $ sort $ M.elems k)
