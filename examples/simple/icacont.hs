import Numeric.LinearAlgebra
import Vision
import Features.Polyline
import EasyVision(Segment(..))
import Util.Rotation(rot3)
import Util.Misc(degree,vec,debug)
import Graphics.Plot
import Data.List(foldl',sortBy)
import Data.Function(on)
import Numeric.GSL.Polynomials(polySolve)

showCont (Closed ps) = mplot x where
    x = toColumns $ fromLists $ map p2l (ps++[head ps])

p2l (Point x y) = [x,y]

dispCont (Closed c) = c

moments = momentsContour.dispCont

trian = Closed $ [
    Point 0 0,
    Point (-1) 2,
    Point 3 0 ]

vshape = Closed $ [
    Point 0 0,
    Point (-1) (-1),
    Point 0 2,
    Point 1 (-1) ]

vshape2 = Closed $ [
    Point 0 (-0.9),
    Point (-1) (-1),
    Point 0 2,
    Point 1 (-1) ]


cross1 = Closed $ reverse [
    Point 0 0,
    Point 1 0,
    Point 1 1,
    Point 2 1,
    Point 2 2,
    Point 1 2,
    Point 1 3,
    Point 0 3,
    Point 0 2,
    Point (-1) 2,
    Point (-1) 1,
    Point 0 1 ]

cross2 = Closed $ reverse [
    Point 0 (-1),
    Point 1 (-1),
    Point 1 1,
    Point 2 1,
    Point 2 2,
    Point 1 2,
    Point 1 3,
    Point 0 3,
    Point 0 2,
    Point (-1) 2,
    Point (-1) 1,
    Point 0 1 ]

testCont = cross2

kurtAlpha2 c = f
  where
    f alpha = kurtAlpha coefs (alpha*degree)
    coefs = kurtCoefs c

kurtAlpha1 = flip kurtAlpha1'
    where kurtAlpha1' alpha = kurtosisX . transPol (rot3 $ alpha * degree)

icaconts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

test = do
    let c = whitenContour' $ transPol (rot3 $ (10)*degree) testCont
    showCont testCont
    showCont c
    print $ orientation testCont
    print $ orientation c
    print $ momentsContour $ dispCont testCont
    print $ momentsContour $ dispCont c
    print $ kurtAlpha1 c 30
    print $ kurtAlpha2 c 30
    mplot [ fromList [kurtAlpha1 c a | a <- [0..360]] ]
    let kur = kurtAlpha2 c
    mplot [ fromList [kur a | a <- [0..360]] ]



main = do
    let w = whitenContour' $ transPol (rot3 $ (10)*degree) testCont
    showCont testCont
    showCont w
    let as = vec [-90..90]
    --mplot [ as, fromList [kurtAlpha1 w a | a <- toList as] ]
    let kur = kurtAlpha2 w
    mplot [ as, fromList [kur a | a <- toList as] ]
    mapM_ showCont (icaconts w)
    let sol = map (/degree) $ icaAngles w
    mapM_ print $ sol
    putStrLn "-----------"
    mapM_ print $ map kur sol



