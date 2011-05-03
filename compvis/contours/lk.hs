{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}

import EasyVision hiding (gx,gy)
import Graphics.UI.GLUT(mainLoop,vertex,renderPrimitive,($=),pointSize,PrimitiveMode(Points),lineWidth)
import Tools
import Control.Arrow
import Control.Applicative
import Util.Misc(diagl,pairsWith,vec,mat,degree,debug,debugMat,arrayOf)
import Features.Polyline
import Data.List(groupBy,sortBy,(\\))
import Data.Function(on)
import Data.Maybe(isJust)
import Numeric.LinearAlgebra(ident,fromLists,fromList,inv,dispf,(<>),(@>),(><),toList,Matrix,asRow,rcond)
import GHC.Float(double2Float)
import Data.Colour.Names as Col
import Numeric.GSL.Special(hyperg_2F1)
import Vision(desp,normat3)
import Util.Rotation(rot3,rot1)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Util.Optimize(optimize)
import Util.Estimation(estimateHomographyRaw)
import Text.Printf(printf)

main = do
    prepare >> proc >> mainLoop

f = transPol (diagl[-0.2,0.2,1]) . whitenContour

-- show pairs of contours
shapes2Browser name sz = examplesBrowser name sz sh
  where
    sh (a,b) = do
        let cs = map fst $ circuits a b
            f c = do
                if orientation c < 0 then setColor 1 0 0 else setColor 0 0 1
                shcont c
        mapM_ f cs
        
        
shapes1Browser name sz = examplesBrowser name sz sh
  where
    sh a = do
        let cs = map fst $ circuits a (transPol (diagl[1.1,0.9,1]) a)
            f c = do
                if orientation c < 0 then setColor 1 0 0 else setColor 0 0 1
                shcont c
        mapM_ f cs
        print (a,(transPol (diagl[1.1,0.9,1]) a))


shapes1BrowserD name sz = examplesBrowser name sz sh
  where
    sh a = do
        let b = transPol (desp (0.02,0.01)) a
        let cs = circuits a b
            f (c,(x,y)) = do
                if orientation c < 0
                    then do
                        setColor' red
                        shcont x
                        setColor' white
                        shcont y
                    else do
                        setColor' blue
                        shcont x
                        setColor' white
                        shcont y
                --print $ map snd $ shareWeights (c,(x,y))
        
        mapM_ f cs
        let h = hess desI a
        print h
        let da = diffCont a b
        print $ map snd da
        let g = grad desI da
        print g
        print $ - inv h <> g

shapes1BrowserT name sz = examplesBrowser name sz sh
  where
    sh a = do
        let b = transPol (diagl[1.1,0.9,1] <> desp (0.02,0.01)<>rot3 (5*degree)) a
            --b = transPol (desp (0.02,0.01)) a
            cs = take 10 $ iterate (warpStep projective a) b
        lineWidth $= 1
        lineWidth $= 1
        setColor' Col.gray
        mapM_ shcont cs
        setColor' white
        shcont a
        setColor' yellow
        shcont (last cs)
        
showEvolution name sz a b = examplesBrowser name sz sh (cs `zip` map show [0..])
  where
    cs = take 20 $ iterate (warpStep displac a) b
    sh c = do
        setColor' white
        shcont a
        setColor' yellow
        shcont c



textAt :: Point -> String -> IO ()
textAt (Point x y) s = text2D (d x) (d y) s
    where d = double2Float

drawPosIndex :: [Point] -> IO ()
drawPosIndex ps = sequence_ $ zipWith textAt ps (map show [0..])
    
proc = do
    p <- map (f . fst) <$> digits
--    p <- return $ map (f . fst) pentominos
    shapes2Browser "shapes" (mpSize 20) (pairsWith (,) p `zip` map show [0..])
    shapes1Browser "other" (mpSize 20) (p `zip` map show [0..])
    shapes1BrowserD "derivative" (mpSize 20) (p `zip` map show [0..])
    shapes1BrowserT "update" (mpSize 20) (p `zip` map show [0..])
    showEvolution "evol" (mpSize 20) cc4 (transPol (rot1 (5*degree) <> diagl[1.1,0.9,1] <> desp (0.02,0.01)<>rot3 (5*degree)) cc4)
    showEvolution "evol2" (mpSize 20) cc4 (transPol (desp (0.9,0.8)) cc4)

----------------------------------------------------------------------
----------------------------------------------------------------------

-- compact expression from http://paulbourke.net/geometry/lineline2d/
cross :: Segment -> Segment -> Maybe Point
cross (Segment (Point x1 y1) (Point x2 y2)) (Segment (Point x3 y3) (Point x4 y4)) = r
  where
    d = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)
    u = ((x4-x3)*(y1-y3)-(y4-y3)*(x1-x3))/d
    v = ((x2-x1)*(y1-y3)-(y2-y1)*(x1-x3))/d
    ok = d /= 0 && 0 < u && u <= 1 && 0 < v && v <= 1
    x = x1 + u*(x2-x1)
    y = y1 + u*(y2-y1)
    r | ok = Just (Point x y)
      | otherwise = Nothing


crossings :: Polyline -> Polyline -> ([(Int,Segment)],[(Int,Segment)],[((Int, Int), Point)])
crossings c1 c2 = (s1,s2,r)
  where
    f = zip [0..] . asSegments
    s1 = f c1 
    s2 = f c2
    r = [ ((i,j),p) | (i,a) <- s1, (j,b) <- s2, Just p <- [cross a b] ]


mkMerge q1 q2 = ((t1,t2), next)
  where
    (s1,s2,crs) = crossings q1 q2
    sq1 = mkMap (fst.fst.head) . groupBy ((==)`on` fst.fst) $ crs
    sq2 = mkMap (snd.fst.head) . groupBy ((==)`on` snd.fst) . sortBy (compare `on` snd.fst) $ crs
    t1 = concatMap (g . h . (id &&& sq1.fst)) s1
    t2 = reverse $ concatMap (g . h . (id &&& sq2.fst)) s2
    a1 = mkAssoc t1
    a2 = mkAssoc t2

    -- intersections at each segment
    mkMap f xs = \k -> IM.findWithDefault [] k m
      where
        m = IM.fromList [ (f x, x) | x <- xs ]

    -- sort intersections at each segment
    h (s@(_,Segment p _), xs) = (s, sortBy (compare `on` distPoints p . snd ) xs )

    -- intercalate intersections in the polyline
    g ((_,Segment p _), []) = [(p,Nothing)]
    g ((_,Segment p _), xs) = [(p,Nothing)] ++ map (snd &&& Just . fst) xs

    -- create lookup function for the index of an intersection
    mkAssoc t = (m M.!)
       where
         m = M.fromList [ (ij,k) | ((p, Just ij),k) <- zip t [0::Int .. ] ]

    next (Left k)  = case t1!!k of
                        (_, Nothing) -> Left $ (k+1) `rem` n1
                        (_, Just ij) -> Right $ (a2 ij + 1) `rem` n2
    next (Right k) = case t2!!k of
                        (_, Nothing) -> Right $ (k+1) `rem` n2
                        (_, Just ij) -> Left  $ (a1 ij +1) `rem` n1
    n1 = length t1
    n2 = length t2
    
    arr1 = arrayOf t1
    arr2 = arrayOf t2
    
    pt (Left k) = arr1 k
    pt (Right k) = arr2 k


mkLoops ((t1,t2),next) = prep $ dropWhile (not . null. snd) $ iterate (goLoop next) ([], map Left [0 .. length t1 -1])
  where
    prep = map (groupBy sameDir . rotR) . fst . head

    rotR xs@(Left _: Right _ : _) = xs
    rotR (x:xs) = rotR (xs++[x]) 

    sameDir (Left _) (Left _) = True
    sameDir (Right _) (Right _) = True
    sameDir _ _ = False

    loopIt next x0 = (x0:) $ fst $ break (== x0) $ tail $ iterate next $ x0

    goLoop next (current, remaining) = (c',r')
      where
        l = loopIt next (head remaining)
        c' = l:current
        r' = remaining \\ l 

loops a b = mkLoops (mkMerge a b)


cc1 = Closed [ Point 1 0, Point 1 4, Point 2 4, Point 2 1, Point 4 1, Point 4 4, Point 5 4, Point 5 0 ]
cc2 = Closed [ Point 0 2, Point 0 3, Point 6 3, Point 6 2]
cc3 = Closed [ Point 0 (-1), Point 0 2, Point 3 2, Point 3 (-1)]
cc4 = Closed [ Point a a, Point a b, Point b b, Point b a]
  where
    a = -0.5
    b = 0.5

((s1,s2),nex) = mkMerge cc1 cc2

test = mapM_ print $ loops cc1 cc2


----------------------------------------------------------------------

qq1 = Closed [Point 0 0, Point 0 3, Point 3 3, Point 3 0]
qq2 = Closed [Point 0 4, Point 4 4, Point 4 0]

----------------------------------------------------------------------

auxMerge :: Polyline -> Polyline -> [((Int, Segment), [((Int, Int), Point)])]
auxMerge q1 q2 = g ([0..] `zip` asSegments q1) $ groupBy ((==)`on`fst.fst) crs
  where
    (_,_,crs) = crossings q1 q2
    g r [] = map (id &&& const []) r
    g ((i,s):ss) t@((((k,j),p):l):ls)
       | i <  k = ((i,s),[]) : g ss t
       | i == k = ((i,s),(((k,j),p):l)) : g ss ls

merge :: Polyline -> Polyline -> [(Point, Maybe Int)]
merge q1 q2 = (rot . f) (auxMerge q1 q2)
  where
    f = concatMap g
    g ((i,Segment p q),[]) = [(p,Nothing)]
    g ((i,Segment p q),l) =  [(p,Nothing)] ++ map (snd &&& Just . snd . fst) (h p l)
    h p = sortBy (compare `on` distPoints p . snd)

    rot ((p,Nothing):rs) = rot $ rs++[(p,Nothing)]
    rot x = x

separ' [p] = []
separ' (p:ps) = (p:as++[head bs]) : separ' bs
  where
    (as,bs) = span (not.isJust.snd) ps 

separ xs = separ' (xs++[head xs])

follow ps xs = (map fst xs, map fst r)
  where
    (p1,_) = head xs
    (p2,_) = last xs
    r = takeWhile (not.(approx p2).fst) . dropWhile (not.(approx p1).fst) $ ps

-- warning: use (Int,Int) identifiers for intersections 
approx p1 p2 = distPoints p1 p2 < 1E-8

rawCircuits a b = map (follow (dup $ merge b a)) (separ $ merge a b)
  where dup x = x++x

-- warning: check tail 
circuits x y = map f (rawCircuits x y)
  where f (a,b) = (Closed $ a ++ reverse (tail b), (Open a, Open (b++[last a])))

shareWeights (c,(a,_)) = map f ss
  where
    w = orientation c
    ss = asSegments a
    l = sum $ map segmentLength ss
    f s = (s, segmentLength s * w / l) 

diffCont a b = concatMap shareWeights (circuits a b)

----------------------------------------------------------------------

data InfoSeg = InfoSeg {
    gx, gy :: Double, mpq :: Int -> Int -> Double }

infoSeg (Segment (Point x1 y1) (Point x2 y2)) = InfoSeg {
    gx = dy/d,
    gy = -dx/d,
    mpq = m }
  where
    dx = x2-x1
    dy = y2-y1
    d = 1 -- sqrt (dx*dx + dy*dy)   -- ???
    m 0 0 = 1
    m 1 0 = (x1+x2)/2
    m 0 1 = (y1+y2)/2
    m 2 0 = (x1*x1+x2*x2+x1*x2)/3
    m 0 2 = (y1*y1+y2*y2+y1*y2)/3
    m 1 1 = (2*x1*y1+2*x2*y2+x2*y1+x1*y2)/6
    m 3 0 = ((x1 + x2)*(x1**2 + x2**2))/4
    m 0 3 = ((y1 + y2)*(y1**2 + y2**2))/4
    m 2 1 = (2*x1*x2*(y1 + y2) + x1**2*(3*y1 + y2) + x2**2*(y1 + 3*y2))/12
    m 1 2 = ((3*x1 + x2)*y1**2 + 2*(x1 + x2)*y1*y2 + (x1 + 3*x2)*y2**2)/12
    m 4 0 = (x1**4 + x1**3*x2 + x1**2*x2**2 + x1*x2**3 + x2**4)/5
    m 0 4 = (y1**4 + y1**3*y2 + y1**2*y2**2 + y1*y2**3 + y2**4)/5
    m 3 1 = (x1**3*(4*y1 + y2) + x1**2*x2*(3*y1 + 2*y2) + x1*x2**2*(2*y1 + 3*y2) + x2**3*(y1 + 4*y2))/20
    m 1 3 = ((4*x1 + x2)*y1**3 + (3*x1 + 2*x2)*y1**2*y2 + (2*x1 + 3*x2)*y1*y2**2 + (x1 + 4*x2)*y2**3)/20
    m 2 2 = (x1**2*(6*y1**2 + 3*y1*y2 + y2**2) + x1*x2*(3*y1**2 + 4*y1*y2 + 3*y2**2) + x2**2*(y1**2 + 3*y1*y2 + 6*y2**2))/30
    m _ _ = 0

-- it can be written in general in terms of hyperg_2F1
gmpq (Segment (Point x1 y1) (Point x2 y2)) n m = 
    ((y1 - y2)**(-1 - n)*
    (x2*y1 - x1*y2)**n*
    (y1**(1 + m)*
    hyperg_2F1 (1 + m) (-n) (2 + m) (((x1 - x2)*y1)/(-(x2*y1) + x1*y2)) - 
    y2**(1 + m)*
    hyperg_2F1 (1 + m) (-n) (2 + m) (((x1 - x2)*y2)/(-(x2*y1) + x1*y2))
    ))/(1 + m)

-------------

hessElem ((s11,n11,m11),(s12,n12,m12)) ((s21,n21,m21),(s22,n22,m22)) (infoSeg -> InfoSeg{..}) = r
  where
    r =
        gx*gx*s11*s21*mpq (n11+n21) (m11+m21) +
        gx*gy*s11*s22*mpq (n11+n22) (m11+m22) +
        gy*gx*s12*s21*mpq (n12+n21) (m12+m21) +
        gy*gy*s12*s22*mpq (n12+n22) (m12+m22)

hess ps c = r
  where
    ss = asSegments c
    r = fromLists [[ sum $ map (hessElem p1 p2) ss | p1 <- ps ] | p2 <- ps ]

gradElem ((s1,n1,m1),(s2,n2,m2)) ((infoSeg -> InfoSeg{..}),err) = r*err
  where
    r = gx*s1*mpq n1 m1 + gy*s2*mpq n2 m2

grad ps dc = r
  where
    r = fromList [sum $ map (gradElem p) dc | p <- ps]
    
z = (0,0,0)
sc_x = ((1,1,0),z)
sc_y = (z,(1,0,1))
d_x = ((1,0,0),z)
d_y = (z,(1,0,0))
sk_x = ((1,0,1),z)
sk_y = (z,(1,1,0))
p_x = ((-1,2,0),(-1,1,1))
p_y = ((-1,1,1),(-1,0,2))


affineI = [sc_x, sc_y, sk_x, sk_y, d_x, d_y]
mktA [a,d,c,b,e,f] = (3><3) [1+a,c,   e,
                             b  ,1+d, f,
                             0  ,  0, 1] :: Matrix Double

affine = (affineI,mktA)

projectiveI = [sc_x, sc_y, sk_x, sk_y, d_x, d_y, p_x, p_y]
mktP [a,d,c,b,e,f,g,h] = (3><3) [1+a,c,   e,
                             b  ,1+d, f,
                             g  ,  h, 1] :: Matrix Double

projectiveI' = [sc_x, sc_y, sk_x, sk_y, d_x, d_y, p_y]
mktP' [a,d,c,b,e,f,h] = (3><3) [1+a,c,   e,
                             b  ,1+d, f,
                             0  ,  h, 1] :: Matrix Double


projectiveI'' = [p_x]
mktP'' [g] = (3><3) [1+0,0,   0,
                     0  ,1+0, 0,
                     g  , 0, 1] :: Matrix Double

projective = (projectiveI, mktP)


desI = [d_x, d_y]
mktD [e,f] = (3><3) [1, 0, e,
                     0, 1, f,
                     0, 0, 1] :: Matrix Double

displac = (desI,mktD)

warpStep (ps,mk) a = f
  where
    h = debugMat "H" 5 id $ hess ps a
    f b = r
      where
        da = debug "diff" ((printf "%.5f" :: Double->String). sum . map (abs.snd)) $ diffCont a b
        g = grad ps da
        x = - inv h <> g
        t = mk (toList $ x)
        r = transPol t b

klpoly a b = estimateHomographyRaw (g b') (g b)
  where
    b' = fst $ optimize 1E-3 0.5 100 m f b
    m = warpStep projective a
    f = sum . map (abs.snd) . diffCont a
    g = map p2l . polyPts
    p2l (Point x y) = [x,y]

h = rot1 (5*degree) <> diagl[1.1,0.9,1] <> desp (0.02,0.01)<>rot3 (5*degree)

h' = normat3 $ klpoly c (transPol h c) <> h
  where
    c = whitenContour cc1

