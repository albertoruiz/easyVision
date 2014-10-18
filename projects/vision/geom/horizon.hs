import Vision.GUI
import Image
import Image.Processing
import Util.Options(optionFromFile,getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra hiding (gjoin)
import Vision(estimateHomography,scaling)
import Util.Geometry as G
import Util.Debug(debug)
--import Contours(bounding, poly2roi)

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    runIt $ do
        p <- clickPoints "click rectangle" "--points" () (sh mbimg.fst)
        w <- browser "horizon" [] (const id)
        connectWith (g mbimg) p w

sh mbimg pts = Draw [ Draw mbimg
                    , color lightgreen . drawPointsLabeled $ pts]

g mbimg (k,_) (ps,_) = (k, [Draw [Draw smbimg, color red [ Draw sls, pointSz 5 sps ]]
                           ])
  where
    [p1,p2,p3,p4] = take 4 ps
    l1 = gjoin p1 p2
    l2 = gjoin p3 p4
    l3 = gjoin p1 p4
    l4 = gjoin p2 p3
    l_inf' = (meet l1 l2) `gjoin` (meet l3 l4)
    ls | length ps >=4 = [l_inf',l1,l2,l3,l4]
       | otherwise = []
    h = unsafeFromMatrix $ scaling (1/3) :: Homography
    sls = h <| ls
    sps = h <| ps
    smbimg = warp  (Word24 50 0 0) (maybe (Size 400 400) size mbimg) (toMatrix h) `fmap` mbimg

