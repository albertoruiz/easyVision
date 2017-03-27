{-# LANGUAGE FlexibleContexts #-}

import Vision.GUI.Simple
import Image.Devel ( pixelsToPoints )
import Image.Processing
import Contours
import Graphics.UI.GLUT
    ( motionCallback, ($=), Position(..), postRedisplay )
import Util.Misc ( memo )
import Vision ( ht, desp )
import Util.Rotation ( rot3 )
import Numeric.LinearAlgebra


main = runIt trazo

sz = Size 600 600

n = 10

trazo = do
    w <- standalone sz "Trazo" ([],[]) updts [] sh
    motionCallback $= Just (mv w)
    return w

sh (xs,[]) = color white (Open xs)
sh (xs,prots) = Draw [ color white (Open xs),
                       drbest
                     ]
  where
    candis = map (align n (prepro xs)) prots
    best = fst $ minimumBy (compare `on` snd) candis
    h = (snd . whitenBoundary . redu) xs
    best' = map l2p $ ht (inv h) (map p2l (smooth n $ best))
    drbest = if (length xs > 10 && h==h && rcond h > 0.001)
        then (color green . lineWd 2) (Open best')
        else Draw ()
                       
updts = [ (key (Char 'x'), \_ _ (xs,prots) -> ([],prots))
        , (key (Char 'a'), \_ _ (xs,prots) -> (xs,prepro xs:prots)) ]

mv w (Position x y) = do
    let [pt] = pixelsToPoints sz [Pixel (fromIntegral y) (fromIntegral x)]
    updateW w (\(xs,prots) -> (pt:xs, prots))
    postRedisplay Nothing

--------------------------------------------------------------------------------

redu = polyPts . reduceDP (1/500) . Open

prepro = feat . fst . whitenBoundary . redu

smooth n = take 51 . polyPts . invFou 100 n

symclose = Closed . (\c -> init c ++ init (reverse c))

feat = memo 30 . fourierPL . symclose

norScale f = g where
    s = sqrt $ sum [ f k * conjugate (f k) | k<-[-10..10] ]
    g 0 = 0
    g k = f k / (5*s)

align n f g = (h, norm_2 (a #> zv - b)) where
    h 0 = g 0 + d
    h k = g k * z
    zv = a <\> b
    [z] = toList zv
    a = fromLists [[g k] | k<-freqs]
    b = fromList [f k | k<-freqs]
    d = f 0 - g 0
    freqs = [-n..(-1)]++[1..n]

whitenBoundary :: [Point] -> ([Point],Matrix Double)
whitenBoundary ps = (wps,t) where
    (mx,my,cxx,cyy,cxy) = momentsBoundary ps -- closed, hmm
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    t = desp (mx,my) <> rot3 (-a) <> diag (fromList [sqrt (l2/l1),1,1]) <> rot3 (a) <> desp (-mx,-my)
    wps = map l2p $ ht t (map p2l ps)

p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

