import Numeric.LinearAlgebra
import Vision
import ImagProc.Base(Polyline(..))
import Contours
import EasyVision(Segment(..),contours,grayscale,readFolder')
import Util.Rotation(rot3)
import Util.Misc(degree,vec,debug,pairsWith)
import Util.Options(optionFromFile)
import Graphics.Plot
import Data.List(foldl',sortBy)
import Data.Function(on)
import Numeric.GSL.Polynomials(polySolve)
import Text.Printf(printf)
import Control.Arrow((***))
import Control.Applicative((<$>))
import qualified EasyVision as EV 

main = main'

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

regular n = Closed [Point (cos t) (sin t) | t <- ts]
  where
    ts = reverse $ tail $ toList $ linspace (n+1) (0,2*pi)
    


rectangle = Closed [Point 0 0, Point 0 1, Point 5 1, Point 5 0]

lshape = Closed [Point 0 0, Point 0 1, Point 3 1, Point 3 2, Point 4 2, Point 4 0]

testCont = Closed $ reverse $ polyPts $ fst (pentominos!!17)
--testCont = regular 6 -- trian


kurtAlpha2 c = f
  where
    f alpha = kurtAlpha coefs (alpha*degree)
    coefs = kurtCoefs c

kurtAlpha1 = flip kurtAlpha1'
    where kurtAlpha1' alpha = kurtosisX . transPol (rot3 $ alpha * degree)

icaconts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

derivCoefs [c0,c1,c2,c3,c4,c5,c6] =
    [       -c1
    , 6*c0-2*c2
    , 5*c1-3*c3
    , 4*c2-4*c4
    , 3*c3-5*c5
    , 2*c4-6*c6
    ,   c5      ]

--------------------------------------------------------------------------------

momAlpha f = flip go
  where
    go alpha = f . transPol (rot3 $ alpha * degree)

skewAlpha1 = momAlpha skewX
fiveAlpha1 = momAlpha fiveX
sixAlpha1 = momAlpha sixX 

--------------------------------------------------------------------------------

auxFive k seg@(Segment (Point x1 y1) (Point x2 y2)) =
     k + (2*x1**5*x2*
           (y1 - y2) + 
          2*x1**4*x2**2*
           (y1 - y2) + 
          2*x1**3*x2**3*
           (y1 - y2) + 
          2*x1**2*x2**4*
           (y1 - y2) + 
          2*x1*x2**5*
           (y1 - y2) - 
          x1**6*
           (5*y1 + 2*y2) + 
          x2**6*(2*y1 + 5*y2))
         /84 

fiveX p = foldl' auxFive 0 (asSegments p) 

auxSix k seg@(Segment (Point x1 y1) (Point x2 y2)) =
     k + (x1**6*x2*(y1 - y2) + 
          x1**5*x2**2*
           (y1 - y2) + 
          x1**4*x2**3*
           (y1 - y2) + 
          x1**3*x2**4*
           (y1 - y2) + 
          x1**2*x2**5*
           (y1 - y2) + 
          x1*x2**6*
           (y1 - y2) - 
          x1**7*(3*y1 + y2) + 
          x2**7*(y1 + 3*y2))/56



sixX p = foldl' auxSix 0 (asSegments p) 

--------------------------------------------------------------------------------

catalog = (read <$> readFile "../../../compvis/contours/mpeg7shapes.txt") >>= optionFromFile "--catalog"

test k = do
    shapes <- catalog :: IO [(Polyline,String)]
    let tc = fst $ shapes!!k
    let c = transPol (rot3 $ (10)*degree) $ whitenContour tc
--    showCont testCont
    showCont c
    print $ orientedArea tc
    print $ orientedArea c
    print $ momentsContour $ dispCont tc
    print $ momentsContour $ dispCont c
    print $ kurtAlpha1 c 30
    print $ kurtAlpha2 c 30
    -- compare empirical and rotated kur
--    mplot [ fromList [kurtAlpha1 c a | a <- [0..360]] ]
    let kur = kurtAlpha2 c
--    mplot [ fromList [kur a | a <- [0..360]] ]
    -- the same for skew
--    mplot [ fromList [skewAlpha1 c a | a <- [0..360]] ]
    -- kur an skew together
    let as = [0..359]
        ks = fromList [kurtAlpha1 c a | a <- as]
        mk = sumElements ks / 360
        sk = fromList [skewAlpha1 c a | a <- as]
        k6 = fromList [sixAlpha1 c a | a <- as] / 10
        mk6 = sumElements k6 / 360
    mplot [ fromList as
          , ks - scalar mk
          , sk
          , fromList [fiveAlpha1 c a | a <- as] / 10
          , k6 - scalar mk6
          ]


main' = do
    let w = transPol (rot3 $ (10)*degree) $ whitenContour testCont
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


showCoefs rot cont = do
    let w = transPol (rot3 $ rot*degree) $ whitenContour cont
    putStrLn "- kur coefs ----------"
    mapM_ (printf "%.2f\n") $ kurtCoefs w
    putStrLn "- deriv coefs ----------"
    mapM_ (printf "%.2f\n") $ derivCoefs $ kurtCoefs w
    putStrLn "- clean coefs ----------"
    mapM_ (printf "%.2f\n") $ cleanPoly $ derivCoefs $ kurtCoefs w
    putStrLn "- clean solutions ----------"
    mapM_ print $ map (atan.recip) . polySolve . cleanPoly $ derivCoefs $ kurtCoefs w
    putStrLn "- raw solutions ----------"
    mapM_ print $ map (atan.recip) . polySolve $ derivCoefs $ kurtCoefs w
    putStrLn "- simple poly ----------"
    mapM_ (printf "%.4f\n") $ (\p -> map (/last p) p) $ kurtCoefs w
    putStrLn "- difangles ----------"
    mapM_ (printf "%.4f\n") $ map (/degree) $ pairsWith (\a b -> abs (a-b)) $ icaAngles w

cleanPoly cs = reverse . dropWhile small . reverse $ cs
  where
    m = maximum (map abs cs)
    small c = abs c / m < eps*100

----------------------------------------------------------------------

f i = map (EV.Closed . map flipx . EV.pixelsToPoints (EV.size i) . douglasPeuckerClosed 2 . fst3) . contours 1 0 128 True . grayscale . EV.channelsFromRGB $ i

fst3 (a,_,_) = a

dup x = EV.resize (EV.Size (2*r) (2*c)) x
  where (EV.Size r c) = EV.size x

flipx (Point x y) = (Point (-x) y)

testDB = do
    x <- map (f.dup *** id) <$> readFolder' "../../../data/shapes/mpeg7ok/"
    let bad = map snd $ filter (null.fst) x
    print bad
    print (length . EV.polyPts . head . fst . head $ x)
    showCont (head.fst.head $ x)
    writeFile "mpeg7ok.txt" (show $ map (head *** id) x)
    return()

