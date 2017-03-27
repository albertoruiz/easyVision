import Vision.GUI
import Image.Processing
import Vision.Camera
import Util.Options(getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra hiding (join, size)
import Image.ROI
import Util.Geometry as G
import Util.Debug(debug)
import Control.Applicative((<*>),(<$>))

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    runIt $ do
        p <- clickPoints "click points" "--points" () (sh mbimg . fst)
        w <- browser "rectified" [] (const id)
        connectWith (g mbimg) p w

sh mbimg pts = Draw [ Draw mbimg
                    , color lightgreen . drawPointsLabeled $ pts]

g mbimg (k,_) (ps,_) = (k, [ dr rec2
                           , dr rec1       
                           ])
  where
    dr im = Draw [ Draw im
                 , Draw (Closed square)
                 , drawPointsLabeled square
                 ]
    rec1 = rectif <$> h <*> mbimg
    rec2 = rectif <$> h <*> (bnd ps <$> mbimg)
    rectif = warp  (Word24 50 0 0) (Size 400 400) . toMatrix
    h | length ps < 4 = Nothing
      | otherwise = Just $ computeHomography square ps

square = [Point a a, Point a b, Point b b, Point b a]
  where
    a = 0.50
    b = -a

bnd ps x | length ps < 4 = x
         | otherwise = modifyROI (const (poly2roi (size x) $ bounding $ Closed ps)) x

