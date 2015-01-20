{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}

module Contours.CLK (
    DeformableContour(..),
    noDeformable,
    LKDResult(..),
    lkd,
    refineProjective,
    refineDeformable,
    homographyFromContours
)where

import Numeric.LinearAlgebra hiding (hess)
import Contours
import Vision.Camera ( computeHomography )
import Util.Homogeneous ( scaling )
import Util.Geometry
    ( Matrixlike(toMatrix), segmentLength, distPoints )
import Util.Misc ( Mat, Vec)
import Util.Debug( debug )
import Util.Optimize ( optimize2 )
import Data.List ( partition, transpose, foldl' )
import Contours.Clipping ( deltaContour )
import Control.Arrow ( Arrow((***)) )


----------------------------------------------------------------------

data DeformableContour = DefCont { dmBase :: Polyline
                                 , dmModes :: [Polyline]
                                 , dmAlphas :: [Double] }

noDeformable p = DefCont p [] []

----------------------------------------------------------------------

shareWeights :: ((t, Double), [Polyline]) -> [(Segment, Double)]
shareWeights ((p,oa),ps) = {-debug "MX" (maximum . map snd) $ -} map f ss
  where
    ss = concatMap asSegments ps
    l = sum $ map segmentLength ss
    f s = (s, clipp maxerr $ oa/l)

maxerr = 1000 -- 1E-2

clipp h = min h . max (-h)


diffCont :: Polyline -> Polyline -> [(Segment, Double)]
diffCont b a = concatMap shareWeights (deltaContour b a)

--------------------------------------------------------------------------------

deltaContourSimple :: Polyline -> Polyline -> [((Polyline,Double),[Polyline])]
deltaContourSimple a b = joinAll (deltaContour a b)
  where
    joinAll = concatMap f
      where
        f ((p,d), xs) = map g xs
          where
            l = sum (map perimeter xs)
            g x = ((p, d * perimeter x / l), [x]) 


-- vienen ordenados los subtramos?

sortDelta :: [((Polyline, Double), [Polyline])] -> [((Polyline, Double), [Polyline])]
sortDelta x = sortD [] x
  where
    extres (_,ps) = (head $ polyPts $ head ps, last $ polyPts $ last ps)
    
    sortD prev [a] = prev ++ [a]
    sortD prev (t:xs) = sortD (prev++[t]) (s:rs)
      where
        (a,b) = extres t
        (ss,rs) = partition f xs
        [s] | length ss == 1 = ss
            | otherwise = error $ "DOUBLE region" ++ show x
        f (extres->(c,d)) = c==b


-- empty result means problems! (change to Maybe?)
sortDeltaM :: [((Polyline, Double), [Polyline])] -> [((Polyline, Double), [Polyline])]
sortDeltaM x = sortD [] x
  where
    extres (_,ps) = (head $ polyPts $ head ps, last $ polyPts $ last ps)
    
    sortD prev [a] = prev ++ [a]
    sortD prev (t:xs) | ok = sortD (prev++[t]) (s:rs)
                      | otherwise = []
      where
        (a,b) = extres t
        (ss,rs) = partition f xs
        ok = length ss == 1
        [s] = ss
        f (extres->(c,d)) = c==b



diffModes :: Polyline -> DeformableContour -> [([Segment], Double)]
diffModes b (DefCont a as _) | null delta = []
                             | otherwise = zip ss e
  where
    --delta = sortDelta $ deltaContourSimple (debug "berror" id b) (debug "aerror" id a)
    delta = sortDeltaM $ deltaContourSimple b a
    da = syncWeights a $ concatMap shareWeights delta
    param = prepModes a delta
    dms = map (asSegments . interpMode param) as
    (s,e) = unzip da
    ss = transpose (s:dms)


syncWeights :: Polyline -> [(Segment, b)] -> [(Segment, b)]
syncWeights x = until ((p==).extreme1.fst.head) rot
      where
        p = head (polyPts x)
        rot (y:ys) = ys++[y]
        extreme1 (Segment p _) = p



prepModes :: Polyline -> [(a, [Polyline])] -> [[Double]]
prepModes base' delta = (map paramVals (separ a b))
  where
    a = cl $ polyPts base
    b = cl $ sync base (odelta delta)
    base | orientedArea base' < 0 = error $ "base needs > orientation: " ++ show base'
         | otherwise = base'

    cl xs = xs++[head xs]
    
    separ [_]       _ = []
    separ (a:b:xs) zs = (ys++[b]) : separ (b:xs) rs
      where
        (ys,rs) = span (/=b) zs

    sync x = until ((p==).head) rot
      where
        p = head (polyPts x)
        rot (x:xs) = xs++[x]

    odelta = concatMap (tail.polyPts). next [] . concatMap snd
      where
        extres = \(Open ps)->(head ps,last ps)
        next prev [a] = prev++[a]
        next prev (t@(extres->(a,b)):xs) = next (prev++[t]) (s:rs)
          where
            ([s],rs) = partition f xs
            f (extres->(c,d)) = c==b

    paramVals ps = map (f a b) qs
      where
        a = head ps
        b = last ps
        qs = init . tail $ ps
        f p q r = distPoints p r / distPoints p q


interpMode :: [[Double]] -> Polyline -> Polyline
interpMode pars mode = Closed (concat ps)
  where
    ms = asSegments mode
    ps = zipWith f ms pars
    f (Segment a b) ts = map (g a b) (0:ts)
      where
        g (Point x1 y1) (Point x2 y2) t = Point (x1+(x2-x1)*t) (y1+(y2-y1)*t)

--------------------------------------------------------------------------------

data InfoSeg = InfoSeg {
    seglen, gx, gy :: Double, 
    mpq :: Int -> Int -> Double,
    cpq :: Double -> Double -> Int -> Int -> Double 
    }


infoSeg (Segment (Point x1 y1) (Point x2 y2)) = InfoSeg {
    seglen = d,
    gx = dy/d,
    gy = -dx/d,
    mpq = \i j -> d* m i j,  -- (length x int_0^1)
    cpq = \d1 d2 i j -> d * c d1 d2 i j }
  where
    dx = x2-x1
    dy = y2-y1
    d = sqrt (dx*dx + dy*dy)
    
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
    
    c d1 d2 0 0 = (d1+d2)/2
    c d1 d2 1 0 = (d1*x1)/3 + (d2*x1)/6 + (d1*x2)/6 + (d2*x2)/3
    c d1 d2 0 1 = (d1*y1)/3 + (d2*y1)/6 + (d1*y2)/6 + (d2*y2)/3
    c d1 d2 1 1 = (d1*x1*y1)/4 + (d2*x1*y1)/12 + (d1*x2*y1)/12 + 
                  (d2*x2*y1)/12 + (d1*x1*y2)/12 + (d2*x1*y2)/12 +
                  (d1*x2*y2)/12 + (d2*x2*y2)/4
    c d1 d2 2 0 = (d1*x1**2)/4 + (d2*x1**2)/12 + (d1*x1*x2)/6 + 
                  (d2*x1*x2)/6 + (d1*x2**2)/12 + (d2*x2**2)/4
    c d1 d2 0 2 = (d1*y1**2)/4 + (d2*y1**2)/12 + (d1*y1*y2)/6 + 
                  (d2*y1*y2)/6 + (d1*y2**2)/12 + (d2*y2**2)/4
    c d1 d2 2 1 = (d1*x1**2*y1)/5 + (d2*x1**2*y1)/20 + (d1*x1*x2*y1)/10 + 
                  (d2*x1*x2*y1)/15 + (d1*x2**2*y1)/30 + (d2*x2**2*y1)/20 + 
                  (d1*x1**2*y2)/20 + (d2*x1**2*y2)/30 + (d1*x1*x2*y2)/15 + 
                  (d2*x1*x2*y2)/10 + (d1*x2**2*y2)/20 + (d2*x2**2*y2)/5
    c d1 d2 1 2 = (d1*x1*y1**2)/5 + (d2*x1*y1**2)/20 + (d1*x2*y1**2)/20 + 
                  (d2*x2*y1**2)/30 + (d1*x1*y1*y2)/10 + (d2*x1*y1*y2)/15 + 
                  (d1*x2*y1*y2)/15 + (d2*x2*y1*y2)/10 + (d1*x1*y2**2)/30 + 
                  (d2*x1*y2**2)/20 + (d1*x2*y2**2)/20 + (d2*x2*y2**2)/5
    c d1 d2 2 2 = (d1*x1**2*y1**2)/6 + (d2*x1**2*y1**2)/30 + (d1*x1*x2*y1**2)/15 + 
                  (d2*x1*x2*y1**2)/30 + (d1*x2**2*y1**2)/60 + (d2*x2**2*y1**2)/60 + 
                  (d1*x1**2*y1*y2)/15 + (d2*x1**2*y1*y2)/30 + (d1*x1*x2*y1*y2)/15 + 
                  (d2*x1*x2*y1*y2)/15 + (d1*x2**2*y1*y2)/30 + (d2*x2**2*y1*y2)/15 + 
                  (d1*x1**2*y2**2)/60 + (d2*x1**2*y2**2)/60 + (d1*x1*x2*y2**2)/30 + 
                  (d2*x1*x2*y2**2)/15 + (d1*x2**2*y2**2)/30 + (d2*x2**2*y2**2)/6
    c d1 d2 n m = error $ show (n,m)
    
{-
-- it can be written in general in terms of hyperg_2F1
gmpq (Segment (Point x1 y1) (Point x2 y2)) n m = 
    ((y1 - y2)**(-1 - n)*
    (x2*y1 - x1*y2)**n*
    (y1**(1 + m)*
    hyperg_2F1 (1 + m) (-n) (2 + m) (((x1 - x2)*y1)/(-(x2*y1) + x1*y2)) - 
    y2**(1 + m)*
    hyperg_2F1 (1 + m) (-n) (2 + m) (((x1 - x2)*y2)/(-(x2*y1) + x1*y2))
    ))/(1 + m)
-}

--------------------------------------------------------------------------------

data ParamDeriv = PTran (Double, Int, Int) (Double, Int, Int)
                | PMode Int

-- the first one is the base polygon, followed by possible modes
-- This is Gauss-Newton (the second derivative is approximated by the product of first derivatives)
-- we integrate the product of derivatives in each section, different from the product of integrals 
hessElem :: ParamDeriv -> ParamDeriv -> [Segment] -> Double

hessElem p@(PMode _) q@(PTran _ _) s = hessElem q p s

hessElem (PTran (s11,n11,m11) (s12,n12,m12)) (PTran (s21,n21,m21) (s22,n22,m22)) ((infoSeg -> InfoSeg{..}) :_) = r
  where
    r = gx*gx*s11*s21*mpq (n11+n21) (m11+m21) +
        gx*gy*s11*s22*mpq (n11+n22) (m11+m22) +
        gy*gx*s12*s21*mpq (n12+n21) (m12+m21) +
        gy*gy*s12*s22*mpq (n12+n22) (m12+m22)

hessElem (PTran (s1,n1,m1) (s2,n2,m2)) (PMode k) ((infoSeg -> InfoSeg{..}): ms) = r
  where
    Segment (Point dx1 dy1) (Point dx2 dy2) = ms!!(k-1)
    r = gx*gx*s1 * cpq dx1 dx2 n1 m1
      + gx*gy*s2 * cpq dx1 dx2 n2 m2
      + gy*gx*s1 * cpq dy1 dy2 n1 m1
      + gy*gy*s2 * cpq dy1 dy2 n2 m2
    
hessElem (PMode i) (PMode j) ((infoSeg -> InfoSeg{..}): ms) = r * seglen
  where
    Segment (Point dx1 dy1) (Point dx2 dy2) = ms!!(i-1)
    Segment (Point ex1 ey1) (Point ex2 ey2) = ms!!(j-1)
    r = gx*gx*(dx1*ex1 + dx1*(ex2-ex1)/2 + (dx2-dx1)*ex1/2 + (dx2-dx1)*(ex2-ex1)/3)
      + gx*gy*(dx1*ey1 + dx1*(ey2-ey1)/2 + (dx2-dx1)*ey1/2 + (dx2-dx1)*(ey2-ey1)/3)
      + gy*gx*(dy1*ex1 + dy1*(ex2-ex1)/2 + (dy2-dy1)*ex1/2 + (dy2-dy1)*(ex2-ex1)/3)
      + gy*gy*(dy1*ey1 + dy1*(ey2-ey1)/2 + (dy2-dy1)*ey1/2 + (dy2-dy1)*(ey2-ey1)/3)


hess :: [ParamDeriv] -> DeformableContour -> Mat
hess ps (DefCont c ms _) = r
  where
    ss = transpose $ map asSegments (c:ms)
    r = fromLists [[ sum $ map (hessElem p1 p2) ss | p1 <- ps ] | p2 <- ps ]

--------------------------------------------------------------------------------

gradElem :: ParamDeriv -> ([Segment],Double) -> Double
-- (grad x err)
gradElem (PTran (s1,n1,m1) (s2,n2,m2)) ((infoSeg -> InfoSeg{..}):_,err) = r*err
  where
    r = gx*s1*mpq n1 m1 + gy*s2*mpq n2 m2

gradElem (PMode k) (s:ms,err) = r*err
  where
    Segment (Point dx1 dy1) (Point dx2 dy2) = ms!!(k-1)
    InfoSeg{..} = infoSeg s
    r = (gx*dxa + gy*dya) * seglen
    dxa = (dx1+dx2)/2
    dya = (dy1+dy2)/2


grad :: [ParamDeriv] -> [([Segment], Double)] -> Vec
grad ps dc = r
  where
    r = fromList [sum $ map (gradElem p) dc | p <- ps]

--------------------------------------------------------------------------------

type Model = ([ParamDeriv], [Double] -> Mat)
    
sc_x, sc_y, sc_c, d_x, d_y, sk_x, sk_y, p_x, p_y :: ParamDeriv

z = (0,0,0)

sc_x = PTran  (1,1,0)     z
sc_y = PTran     z     (1,0,1)
sc_c = PTran  (1,1,0)  (1,0,1)
d_x  = PTran  (1,0,0)     z
d_y  = PTran     z     (1,0,0)
sk_x = PTran  (1,0,1)     z
sk_y = PTran     z     (1,1,0)
p_x  = PTran (-1,2,0) (-1,1,1)
p_y  = PTran (-1,1,1) (-1,0,2)


affineI = [sc_x, sc_y, sk_x, sk_y, d_x, d_y]
mktA [a,d,c,b,e,f] = (3><3) [1+a,c,   e,
                             b  ,1+d, f,
                             0  ,  0, 1] :: Matrix Double

affine :: Model
affine = (affineI,mktA)

projectiveI = [sc_x, sc_y, sk_x, sk_y, d_x, d_y, p_x, p_y]
mktP [a,d,c,b,e,f,g,h] = (3><3) [ 1+a, c,   e,
                                  b  , 1+d, f,
                                  g  ,   h, 1] :: Matrix Double
projective :: Model
projective = (projectiveI, mktP)


desI = [d_x, d_y]
mktD [e,f] = (3><3) [1, 0, e,
                     0, 1, f,
                     0, 0, 1] :: Matrix Double

displac :: Model
displac = (desI,mktD)

scaledespI = [sc_c, d_x, d_y]
mktSD [s,e,f] = (3><3) [1+s,  0, e,
                        0,  1+s, f,
                        0,    0, 1] :: Matrix Double

scaledesp :: Model
scaledesp = (scaledespI,mktSD)

--------------------------------------------------------------------------------

warpStep :: Model -> Polyline -> (Mat,Polyline) -> ((Mat,Polyline),Double)
warpStep (ps,mk) a = f
  where
    h = {- debugMat "H" 2 id $ -} hess ps (DefCont a [] [])
    mih = - inv h
    f (s,b) = ((t<>s,r), {- debug "diff" id . -} sum . map (abs.snd) $ da)
      where
        da = map (return *** id) $ diffCont b a
        g = grad ps da
        x = mih <> g
        t = mk (toList $ x)
        r = transPol t b


warpStepDef :: Model -> DeformableContour -> Polyline -> (Mat,[Double])
warpStepDef (pars,mk) p@(DefCont a as _) b | null da = debug "HORROR"  (length.snd) (ident 3, replicate (length as) 0)
                                           | otherwise = (t,p2)
  where
    param = pars++ map PMode [1..length as]
    h = -- debugMat "H" 2 id $
        hess param p
    mih = - inv h

    da = diffModes b p
    
    g = -- debug "G" id $
        grad param da
    x = mih <> g
    --x = - 0.5 * g
    (p1,p2) = splitAt (length pars) (toList x)
    t = mk p1

--------------------------------------------------------------------------------


zipPolyWith f (Closed ps) (Closed qs) = Closed (zipWith f ps qs)
zipPolyWith f (Open ps) (Open qs) = Open (zipWith f ps qs)
zipPolyWith f _ _ = error "zipWithPoly Open-Closed"


(~+~) = zipPolyWith (.+.)
  where
    Point x1 y1 .+. Point x2 y2 = Point (x1+x2) (y1+y2)
        
s ~*~ p = transPol (scaling s) p

stepDef (img, p@(DefCont prot modes accAlphas)) = (img', q)
  where
     (hh,alphas) = warpStepDef projective p img
     prot' = foldl' (~+~) prot $ zipWith (~*~) (map negate alphas) modes
     img' = transPol hh img
     q | orientedArea prot' > 0 = DefCont prot' modes (zipWith (subtract) alphas accAlphas)
       | otherwise = p






--------------------------------------------------------------------------------


data LKDResult = LKDResult
    { lkdSource     :: Polyline  -- ^ protype, possibly deformed
    , lkdAlignment  :: Polyline  -- ^ prototype aligned to target
    , lkdHomography :: Mat
    , lkdAlphas     :: [Double]
    , lkdErrors     :: [Double]
    }


lkd :: DeformableContour         -- ^ prototype
    -> Polyline                  -- ^ target
    -> LKDResult

lkd (DefCont a [] _) b = LKDResult{..}
  where
    ((lkdHomography,lkdAlignment),lkdErrors) = optimize2 1E-3 1 10 (warpStep projective a) (ident 3, b)
    lkdAlphas = []
    lkdSource = a


lkd model x = LKDResult{..}
  where
    (imagInvTrans, DefCont baseDef _ lkdAlphas) = last $ take 10 $ iterate stepDef (x, model)
    lkdHomography = toMatrix $ computeHomography (polyPts x) (polyPts imagInvTrans)
    lkdErrors = [0,0] -- FIXME
    lkdAlignment = transPol lkdHomography baseDef
    lkdSource = baseDef



-- h takes proto to target, giving xor err
refineProjective ShapeMatch{..} = (lkdHomography<>wa , head lkdErrors, lkdAlignment)
  where
    a = shapeContour target
    b = transPol wa $ shapeContour proto
    LKDResult{..} = lkd (noDeformable a) b

-- starting from affine alignment
homographyFromContours dst src =
    case md of
        Just d  -> refineProjective (affine d)
        Nothing -> error $ "homographyFromContours cannot initialize target" ++ show dst
  where
    md = shape 10 dst
    ms = shape 10 src
    affine = case ms of
        Just s -> head . shapeMatches [(s, "")]
        Nothing -> error $ "homographyFromContours cannot initialize source" ++ show src

--------------------------------------------------------------------------------

refineDeformable ShapeMatch {..} model = r { lkdAlignment = align, lkdHomography = h }
  where
    imag = transPol (inv wa) $ shapeContour target
    r = lkd model imag
    h = wa <> lkdHomography r
    align = transPol wa (lkdAlignment r)

--------------------------------------------------------------------------------

