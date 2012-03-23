import Vision.GUI
import ImagProc
import Util.Options(optionFromFile,getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra hiding (join)
import Vision(estimateHomography,scaling)
import Util.Geometry as G
import Util.Misc(debug)
import Contours(bounding, poly2roi)

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    runIt $ do
        p <- clickPoints' "click rectangle" "--points" (sh mbimg)
        w <- browser "horizon" [] (const id)
        connectWith (g mbimg) p w

sh mbimg pts = Draw [ Draw mbimg
                    , color lightgreen . drawPointsLabeled $ pts]

g mbimg (k,_) ps = (k, [Draw [Draw smbimg, color red [ Draw sls, pointSz 5 sps ]]
                       ])
  where
    [p1,p2,p3,p4] = take 4 ps
    l1 = join p1 p2
    l2 = join p3 p4
    l3 = join p1 p4
    l4 = join p2 p3
    l_inf' = (meet l1 l2) `join` (meet l3 l4)
    ls | length ps >=4 = [l_inf',l1,l2,l3,l4]
       | otherwise = []
    h = unsafeFromMatrix $ scaling (1/3) :: Homography
    sls = h <| ls
    sps = h <| ps
    smbimg = warp  (50,0,0) (maybe (Size 400 400) size mbimg) (toMatrix h) `fmap` mbimg

