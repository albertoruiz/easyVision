{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

module Tools(
    -- * Invariant features
    maxFreq,
    toCanonicAll, toCanonic2, canonical,
--    protos, 
    protoOri,
--    foufeat, 
    toFun, 
--    prec,
    icaConts,
--  icaContsAll, icaConts2,
    alignment, refine,
    classifier, classifyMon, AlignInfo, elongated,
    alignMon, alignMon',
    -- * Projective rectification from circles and vertical vanishing point
    rectifyMon, tryMetric, rectifierFromHorizon, coherent, distImage,
    rectifierFromAffineHomogs, refineTangent, refineTangentImage,
    -- * Digit tools
    fixOrientation, horizontalGroups, drawTree,
    -- * Other tools
    intersectionManyLines, mseLine, pt2hv, hv2pt,
    -- * Display
    shapeCatalog, normalShape, centerShape, boxShape,
    examplesBrowser, imagesBrowser, shapesBrowser,
    shcont, shcontO,
    rotAround,
    -- * Prototypes
    letters, digits, digits'
) where


import EasyVision as EV
import Graphics.UI.GLUT hiding (Point,Size,scale,samples)
import Util.Misc(diagl,degree,Vec,vec,norm,debug,diagl,memo,Mat,mat,norm,
                 unionSort,replaceAt,mean,median,unitary,pairsWith,kruskal,neigh)
import Util.Estimation(homogSolve)
import Util.Covariance(meanV,covStr)
import Util.Rotation(rot3,rot1)
import Control.Arrow
import Control.Applicative((<$>),(<*>))
import Control.Monad(when,ap)
import Data.Colour.Names as Col
import Vision (desp, scaling, estimateHomography, ht, cross, homog,
               inHomog,circularConsistency,focalFromCircularPoint,similarFrom2Points,
               kgen,mS, cameraFromAffineHomogZ0,estimateFundamentalRaw)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4,partition,transpose,foldl',group)
import Data.Function(on)
import Text.Printf(printf)
import Data.IORef
import Util.Options
import Util.Ellipses
import Vision.Camera(rectifierFromCircularPoint,imagOfCircPt,selectSol,getHorizs)

import Data.Complex
import Classifier(Sample)
import Data.Maybe(isJust,catMaybes)
import Numeric.GSL.Minimization


----------------------------------------------------------------------

$(autoParam "SCParam" "classify-"
  [( "epsilon","Double" ,realParam 0.2 0 1),
   ( "maxangle"  ,"Double" ,realParam 5 0 20)]
 )

----------------------------------------------------------------------

-- must be a parameters
maxFreq = 10

type AlignInfo = (Double,(Polyline,Mat,Vec,Mat,Vec,String))

alignment :: Sample (Mat,Vec) -> Polyline -> AlignInfo
alignment prots x = minimumBy (compare `on` fst) $ [basicDist x z p | z <- zs, p <- prots]
  where
    zs = toCanonicAll x
    basicDist x (b,u) ((a,v),l) = (dist u v, (x,b,u,a,v,l))

refine :: Double -> AlignInfo -> AlignInfo
refine amax (d, (x,b,u,a,v,l)) = (d',(x, b',u',a,v,l))
  where
    alpha = minimumBy (compare `on` dist v . ufr) angs
    uf = toFun u
    ufr ang = prec maxFreq $ normalizeStart $ (cis ang *) . uf
    angs = map (* degree) [-amax .. amax]
    d' = dist (ufr alpha) v
    u' = ufr alpha
    b' = rot3 (-alpha) <> b

dist a b = norm (a-b)
    
----------------------------------------------------------------------

classify SCParam {..} (im,cs,prots) = (im, oks)
  where
    oks = filter ((<epsilon).fst) (map clas cs) 
    clas = refine maxangle . alignment prots

classifier = classify .@. winSCParam

----------------------------------------------------------------------

classifyMon cam = monitor "Detected" (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (f.g) oks
    g (d, (x,b,u,a,v,l)) = (d,((x, inv b <> a),l))
    f (d,((c,m),l)) = do
        setColor' white
        shcont c
        text2D (d2f ox) (d2f up) l -- (printf "%s %.0f" l (d*100))
        setColor' yellow
        shcont $ (transPol m) (Closed [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1])
        setColor' red
        shcont $ (transPol m) (Closed [Point 0 0, Point (-2) 0, Point 0 0, Point 0 2])
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)
        up = 2/400 + maximum (map py (polyPts c))

d2f = fromRational.toRational

----------------------------------------------------------------------

-- only alignment
alignMon cam = monitor "Alignment" (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (f.g) oks
    g (d, (x,b,u,a,v,l)) = (b,(u,v))
    f (mb,(cx,cb)) = do
        lineWidth $= 1
        let al = transPol (inv mb) (invFou 100 maxFreq $ toFun cb)
            ob = transPol (inv mb) (invFou 100 maxFreq $ toFun cx)
            corr = zipWith (\a b->[a,b]) (polyPts al) (polyPts ob)
        setColor' yellow
        let aux (Closed ps) = map p2l ps
            p2l (Point x y) = [x,y]
        renderPrimitive Lines $ mapM_ vertex (concat corr)
        lineWidth $= 1
        setColor' red
        shcont al

----------------------------------------------------------------------

-- alignment and classification
alignMon' name cam = monitor name (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (f.g) oks
    g (d, (x,b,u,a,v,l)) = (d,x,(b,(u,v)),l)
    f (d,x,(mb,(cx,cb)),l) = do
        lineWidth $= 1
        let al = transPol (inv mb) (invFou 100 maxFreq $ toFun cb)
            ob = transPol (inv mb) (invFou 100 maxFreq $ toFun cx)
            corr = zipWith (\a b->[a,b]) (polyPts al) (polyPts ob)
        setColor' yellow
        let aux (Closed ps) = map p2l ps
            p2l (Point x y) = [x,y]
        renderPrimitive Lines $ mapM_ vertex (concat corr)
        lineWidth $= 1
        setColor' red
        shcont al
        setColor' black
        text2D (d2f ox) (d2f up) (printf "%s %.0f" l (d*100))
      where
        (ox,oy,_,_,_) = momentsContour (polyPts x)
        up = 2/400 + maximum (map py (polyPts x))

----------------------------------------------------------------------

    
fixOrientation :: (a, [AlignInfo]) -> (a, [AlignInfo])
fixOrientation (im, xs) = (im, zs ++ ys)
  where
    (zs, qs)  = partition asym xs
      where
        asym (_, (_,_,_,_,_,l)) = l `elem` ["1","2","3","4","5","7"]
    
    ys = if null zs then qs else map (f dx dy) qs
    
    [dx,dy] = map mean $ transpose (map g zs)
      where
        g (_, (_,b,_,a,_,_)) = dir $ ht (inv b <> a) [[0,0],[1,0]]
    
    dir [[ox,oy],[x,y]] = [x-ox,y-oy]
    
    f dx dy (d, (x,b,u,a,v,l)) | l `elem` ["$","6","9","8"] && flipped = (d, (x,b,u, a<>rot3 pi, v, comp l))
                               | l == "0" = (d, (x,b,u, rot3 angle <> a, v,l))
                               | otherwise = (d, (x,b,u,a,v,l))
      where
        [[cx,cy],diry ] = ht (inv b <> a) [[0,0],[1,0]]
        [r,s] = dir [[cx,cy],diry]
        dot = r*dx + s*dy
        flipped = dot < 0

        [[xx,xy]] = ht (inv a <> b) [[cx+dx,cy+dy]]
        angle = - atan2 xy xx

        comp "6" = "9"
        comp "9" = "6"
        comp x = x
    
----------------------------------------------------------------------

-- find groups of "horizontally" aligned shapes
-- we obtain the connected components of the spanning tree of distances between shapes
-- the distance is the minimum between the center of a shape and the +/3 widths of the other

drawTree dt = do
    setColor' white
    renderPrimitive Lines $ mapM_ vertex dt
    setColor' red
    pointSize $= 3
    renderPrimitive Points $ mapM_ vertex dt


tree :: [AlignInfo] -> ([(Int,Int)],[Point])
tree xs = (t, concatMap h t)
  where
    t = kruskal (length xs-1) arcs
    arcs = map snd $ filter ((<0.2).fst) $ sort $ pairsWith f (ys `zip` [0..])

    ys = map g xs
    g  (d, (x,b,u,a,v,l)) = (Point cx cy, Point x1 y1, Point x2 y2)
        where [[cx,cy],[x1,y1],[x2,y2]] = ht (inv b <> a) [[0,0],[2,0],[-2,0]]

    f ((p,p1,p2),i) ((q,q1,q2),j) = (d,(i,j))
      where
        d = min (distPoints p q1 + distPoints q p2)
                (distPoints p q2 + distPoints q p1)

    h (i,j) = [Point cx cy, Point dx dy]
      where
        (Point cx cy,_,_) = ys!!i
        (Point dx dy,_,_) = ys!!j 


connected nmax arcs = fixedPoint grow (map return [0..nmax])
  where
    fixedPoint f x0 = let x1 = f x0 in if x1 == x0 then x0 else fixedPoint f x1
    grow = unique . map (unique . concatMap neigh')
    neigh' x = x : neigh arcs x
    unique :: Ord t => [t] -> [t]
    unique = map head . group . sort

horizontalGroups sel = (dt,gs)
  where
    (t, dt) = tree sel
    con = filter ((>2).length) $ connected (length sel -1) t
    gs = map (map (sel!!)) con


----------------------------------------------------------------------

elongated :: Double -> Polyline -> Bool
elongated r p = sqrt l2 / sqrt l1 < 1/r 
  where
    (l1,l2,_) = eig2x2Dir (cxx,cyy,cxy)
    (mx,my,cxx,cyy,cxy) = momentsContour $ polyPts p

----------------------------------------------------------------------

-- | smoothed frequential invariant representations (all for the unknow object) with alignment info
toCanonicAll :: Polyline -> [(Mat,Vec)]
toCanonicAll p = zip (map (<>w) cs) cps
  where
    w = rot3 (20*degree) <> whitener p
    wp = transPol w p
    cs = map rot3 (icaAnglesAll wp)
    cps = map (foufeat .flip transPol wp) cs

-- | smoothed frequential invariant representations (the main two, for the prototypes) with alignment info
toCanonic2 :: (Polyline,String) -> [((Mat,Vec),String)]
toCanonic2 (p,l) = zip (map (<>w) cs) cps `zip` repeat l
  where
    w = rot3 (20*degree) <> whitener p
    wp = transPol w p
    cs = map rot3 (icaAngles2 wp)
    cps = map (foufeat .flip transPol wp) cs


-- | whitened with natural orientation, and additional angles
canonical :: Polyline -> (Polyline, Mat, [Double])
canonical p = (r, n, map (subtract a) as)
  where
    w = rot3 (20*degree) <> whitener p  -- check solver
    wp = transPol w p
    a:as = icaAnglesAll wp
    c = rot3 a -- max kurt if orientation ok
    n = c <> w
    r = transPol n p


protoOri :: Sample Polyline -> Sample Polyline
protoOri = concatMap (repSample . (f *** id))
  where
    f = icaConts2 . whitenContour . transPol (rot3 (10*degree)) -- hmm
    repSample (xs,l) = map (id &&& const l) xs


protos ::  Sample Polyline -> Sample Vec
protos = map (foufeat *** id)


foufeat = prec maxFreq .  normalizeStart . fourierPL


prec n f = join[r,c]
    where 
      (r,c) = fromComplex $ fromList $ map f ([-n..n])


toFun cb k = cb@>(k+n) :+ cb@>(k+3*n+1)
    where n = (dim cb - 2) `div` 4

----------------------------------------------------------------------

icaConts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

icaContsAll w = map g (icaAnglesAll w)
  where
    g a = transPol (rot3 a) w

icaConts2 w = map g (icaAngles2 w)
  where
    g a = transPol (rot3 a) w

icaAnglesAll w = as ++ map (+pi) as
  where as = icaAngles w

icaAngles2 w = [head as, last as]
  where as = icaAngles w

----------------------------------------------------------------------
----------------------------------------------------------------------

shapeCatalog prepro gprot feat' cam = do
    raw <- gprot
    let feat = feat' . map (prepro *** id)
        prototypes = feat raw
    (cam',ctrl) <- withPause cam
    w <- evWindow (raw,prototypes,Nothing,False) "Contour Selection" (mpSize 10) Nothing (marker (kbdcam ctrl))
    s <- shapesBrowser "Shape" (EV.Size 240 240) raw
    return $ do
        (raw',prots',click,save) <- getW w
        (im,cs) <- cam'
        let sz = size im -- required to avoid space leak !?
            (raw,prots) = case click of
                Nothing -> (raw',prots')
                Just pix  -> let [pt] = pixelsToPoints sz [pix]
                                 newc = (normalShape $ closestTo pt cs, "?" ++ show (length prots' + 1))
                           in (newc:raw', feat [newc] ++ prots')
        when (isJust click) $ putW s (0, raw) >> postRedisplay (Just (evW s))
        inWin w $ do
            drawImage' im
            pointCoordinates sz
            text2D 0.9 0.6 $ show (length cs)
            setColor' orange
            mapM_ shcont cs
            
        when save $ writeFile "shapes.txt" (show raw)
        putW w (raw,prots,Nothing,False)
        return (im,cs,prots)
  where
    marker _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
        (r,p,_,_) <- readIORef st
        let clicked = Pixel (fromIntegral y) (fromIntegral x)
        writeIORef st (r,p, Just clicked, False)

    marker _ st (Char 'S') Down _ _ = do
        (r,p,_,_) <- readIORef st
        writeIORef st (r,p, Nothing, True)

    marker def _ a b c d = def a b c d

    closestTo pt = minimumBy (compare `on` (d pt))
      where
        d p c = distPoints p (cen c)
        cen (Closed c) = Point cx cy where (cx,cy,_,_,_) = momentsContour c


shapesBrowser :: String -> EV.Size -> Sample Polyline -> IO (EVWindow (Int, Sample Polyline))
shapesBrowser name sz = examplesBrowser name sz f
  where
    f = shcont . transPol (diagl[-0.2,0.2,1]) . whitenContour

----------------------------------------------------------------------

testLoad path = do
    xs <- readFolder' path
    prepare >> imagesBrowser "Images" (mpSize 20) xs >> mainLoop


testLoad' imgfile = do
    x <- loadRGB imgfile
    run $ return (return x) >>= observe "loaded" id

----------------------------------------------------------------------

shcont c@(Closed _) = renderPrimitive LineLoop (vertex c)

shcont c@(Open _) = renderPrimitive LineStrip (vertex c)

shcontO (Closed c) = do
    pointCoordinates (EV.Size 500 500)
    renderPrimitive LineLoop $ mapM_ vertex c
    pointSize $= 6
    renderPrimitive Points $ vertex (c!!0)
    pointSize $= 4
    renderPrimitive Points $ vertex (c!!1)
    --print c

----------------------------------------------------------------------

centerShape c = transPol h c -- (h<>rotAround x y (-pi/2)) c
 where
   (x,y,_,_,_) = momentsContour (polyPts c)
   h = desp (-x,-y)

normalShape c = transPol h c -- (h<>rotAround x y (-pi/2)) c
 where
   (x,y,sx,sy,_) = momentsContour (polyPts c)
   h = scaling (1/ sqrt( max sx sy)) <> desp (-x,-y)

rotAround x y a = desp (x,y) <> rot3 a <> desp (-x,-y)

boxShape c = transPol h c
  where
    Closed [Point x2 y2, _, Point x1 y1, _] = bounding c
    h = scaling (2/(y2-y1)) <> desp (-(x1+x2)/2,-(y1+y2)/2)
  
----------------------------------------------------------------------

mt m = trans (inv m)

----------------------------------------------------------------------

-- hmm, algebraic error
-- [Vec3]->HPoint2
intersectionManyLines :: [Vec] -> Vec
intersectionManyLines ls | length ls > 1 = fst (homogSolve a)
                         | otherwise = vec [0,0,1]
  where a = fromRows $ map unitary ls

-- [Point2] -> Line2
mseLine :: [Point] -> [Double]
mseLine ps
    | length ps < 2 = error "mseLine with < 2 points"
    | otherwise = (toList . fst) (homogSolve a)
  where
    a = fromRows (map pt2hv ps)

----------------------------------------------------------------------

rectifier rho yh f = kgen f <> rot1 (atan2 f yh) <> rot3 (-rho) <> kgen (recip f)

rectifierFromHorizon f h = rectifier rho yh f
  where
    dn = mS <> h
    l0n = cross dn (vec [0,0,1])
    nh = cross h l0n
    Point x y = hv2pt nh
    yh = sqrt (x*x+y*y)
    rho = atan2 x y

pt2hv (Point x y) = vec [x,y,1]
hv2pt v = Point x y where [x,y] = toList (inHomog v)

----------------------------------------------------------------------

rectifyMon sz cam = do wr <- evWindow () "Rectify omega" (Size 400 400)Nothing (const kbdQuit)
                       monitor "Ellipses" sz (sh wr) cam
  where
    g (d, (x,b,u,a,v,l)) = (d,((x, inv b <> a),l))
    h (d,((c,m),l)) = trans (inv m) <> diagl [1,1,-1] <> (inv m)
    f (d,((c,m),l)) = do
        let ellip = transPol m unitCircle
            ((mx,my,dx,dy,alpha),_) = analyzeEllipse (estimateConicRaw $ polyPts ellip)
        setColor' red
        shcont ellip
        shcont $ transPol (rotAround mx my (-alpha)) (Closed [Point (mx+1*dx) my, Point (mx-1*dx) my])
        shcont $ transPol (rotAround mx my (-alpha)) (Closed [Point mx (my+1*dy), Point mx (my-1*dy)])
 
    sh wr (im,oks) = when (length oks >= 2) $ do
        drawImage im
        pointCoordinates (mpSize 10)
        mapM_ (f.g) oks        
        let gc (d, (x,b,u,a,v,l)) = d -- abs (orientation x)
            ellipMat = map (h.g) (sortBy (compare `on` gc) oks)
            ellipses = map (fst.analyzeEllipse) ellipMat
            improve = id
            sc = 0.5
            orig = im
        when (length ellipMat >= 2) $ do
            let sol = intersectionEllipses (ellipMat!!0) (ellipMat!!1)
                (mbij,mbother) = selectSol (ellipses!!0) (ellipses!!1) sol
            when (isJust mbij && isJust mbother) $ do
                let Just ijr    = mbij
                    ij = improve ijr
                    Just other = mbother
                    [h1,h2] = getHorizs [ij,other]
                lineWidth $= 1
                setColor' Col.blue
                --shLine h1
                setColor' Col.yellow
                --shLine h2
                pointSize $= 5
                setColor' Col.purple
                --renderPrimitive Points $ mapM (vertex.map realPart.t2l) [ij,other]
                setColor' Col.green
                --mapM_ shLine $ map (map realPart) $ tangentEllipses (ellipMat!!0) (ellipMat!!1)
                
                let cc = inv (ellipMat!!0) <> (ellipMat!!1)
                    vs = map (fst . fromComplex) . toColumns . snd . eig $ cc
                setColor' Col.white
                renderPrimitive Points (mapM (vertex . toList. inHomog) vs)

                let ccl = (ellipMat!!0) <> inv (ellipMat!!1)
                    vsl = map (fst . fromComplex) . toColumns . snd . eig $ ccl
                --mapM_ (shLine.toList) vsl



                let recraw = rectifierFromCircularPoint ij
                             --rectifierFromManyCircles id (ellipMat)
                    nsz (_,_,dx,dy,_) = -dx-dy
                    (mx,my,_,_,_):(mx2,my2,_,_,_):_ = sortBy (compare `on` nsz) ellipses
                    [[mx',my'],[mx'2,my'2]] = ht recraw [[mx,my],[mx2,my2]]
                    -- okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [0,0] [-0.5, 0] <> recraw
                    okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [mx,my] [mx2, my2] <> recraw
                
                    --drawImage $ warp 0 sz' (scaling sc <> w) (grayscale orig)
                inWin wr $ drawImage $ warp 0 (Size 400 400) (scaling sc <> okrec ) ( orig)
                --text2D 30 30 $ show $ focalFromHomogZ0 (inv recraw)
                -- it is also encoded in the circular points
                text2D 30 30 $ printf "f = %.2f" $ focalFromCircularPoint ij
                text2D 30 50 $ printf "ang = %.1f" $ abs ((acos $ circularConsistency ij)/degree - 90)

unitCircle = Closed [Point (cos (t*degree)) (sin (t*degree)) | t <- [0,10 .. 350]]

tangentEllipses c1 c2 = map ((++[1]). t2l) $ intersectionEllipses (inv c1) (inv c2)

t2l (a,b) = [a,b]

shLine [a,b,c] = renderPrimitive Lines $ mapM f [-1,1]
    where f x = vertex $ Point x ((-a*x-c)/b)


----------------------------------------------------------------------

tryMetric (im,oks) = (im,oks')
  where
    oks' = map f oks
    g (d, (x,b,_,a,_,l)) = (d,((x, inv b <> a),l))
    h (_,((_,m),_)) = trans (inv m) <> diagl [1,1,-1] <> (inv m)
    gc (d, (x,_,_,_,_,l)) = d -- abs (orientation x)
    ellipMat = map (h.g) (sortBy (compare `on` gc) oks)
    ellipses = map (fst.analyzeEllipse) ellipMat
    sol = intersectionEllipses (ellipMat!!0) (ellipMat!!1)
    (mbij,_) = selectSol (ellipses!!0) (ellipses!!1) sol
    Just ij = mbij
    recraw = rectifierFromCircularPoint ij
    nsz (_,_,dx,dy,_) = -dx-dy
    (mx,my,_,_,_):(mx2,my2,_,_,_):_ = sortBy (compare `on` nsz) ellipses
    [[mx',my'],[mx'2,my'2]] = ht recraw [[mx,my],[mx2,my2]]
    okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [mx,my] [mx2, my2] <> recraw
    rectif = if length oks > 1 && isJust mbij then okrec else ident 3
    f (d, (x,b,u,a,v,l)) = (d', (x,b'<>rectif,u',a',v',l'))
      where
        (d', (x',b',u',a',v',l')) = refine 10 $ alignment [((a,v),l)] (transPol rectif x)

----------------------------------------------------------------------

rectifierFromAffineHomogs f hs = rectifierFromVerticals f ls
  where
    ls = map (g . cameraFromAffineHomogZ0 f ) hs  -- Warning...
    g c = homog (vec a) `cross ` homog (vec b)
      where [a,b] = ht c [[0,0,0],[0,0,1]] -- or select columns
          

rectifierFromVerticals f ls = (v,rectifierFromHorizon f h)
  where
    v = intersectionManyLines ls
    w = diagl [1,1,f**2]
    h = w<>v

----------------------------------------------------------------------
    
distImage (im,oks) = (im,oks')
  where
    oks' = map f oks
    f (d, r@(x,b,u,a,v,l)) = (d',r)
      where
        xf = foufeat x
        pf = foufeat (transPol (inv b) $ invFou 30 maxFreq $ toFun v)
        d' = 10* norm (xf-pf) -- / norm xf

----------------------------------------------------------------------

samples = 100
    
p2l (Point x y) = [x,y]    
    
refineTangent (im,oks) = (im,oks')
  where
    oks' = map f oks
    f (d, (x,b,u,a,v,l)) = (d',(x,b',u',a,v,l))
      where
        xr = invFou samples maxFreq $ toFun u
        pr = invFou samples maxFreq $ toFun v
        g a b = unitary $ cross a b
        cpr = z ++ [head z]
          where z = map pt2hv (polyPts pr)
        ls = zipWith g cpr (tail cpr)
        pl2l = map p2l . polyPts
        ls' = ls
        xr' = map (unitary.homog.vec) $ pl2l xr
        h = estimateHomographyLinesPoints ls' xr'
        b' = h <> b
        u' = foufeat $ transPol b' x
        d' = norm (u'-v)


refineTangentImage (im,oks) = (im,oks')
  where
    oks' = map f oks
    f (d, (x,b,u,a,v,l)) = (d',(x,b',u',a,v,l))
      where
        xr = invFou samples maxFreq $ toFun $ foufeat x
        pr = invFou samples maxFreq $ toFun $ foufeat (transPol (inv b) $ invFou samples maxFreq $ toFun v)
        g a b = unitary $ cross a b
        cpr = z ++ [head z]
          where z = map pt2hv (polyPts pr)
        ls = zipWith g cpr (tail cpr)
        pl2l = map p2l . polyPts
        ls' = ls
        xr' = map (unitary.homog.vec) $ pl2l xr
        h = estimateHomographyLinesPoints ls' xr'
        b' = b <> h
        u' = foufeat $ transPol b' x
        d' = norm (u'-v)


estimateHomographyLinesPoints :: [Vec] -> [Vec] -> Mat
estimateHomographyLinesPoints ls ps = f where
    f = reshape 3 $ fst $ homogSolve (fromRows eqs)
    eqs = zipWith eq ls ps where
     eq l p = flatten (outer l p)

----------------------------------------------------------------------

coherent cam = monitor "Coherent" (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        f (map g oks)
    g (d, (x,b,u,a,v,l)) = inv b <> a
    f hs = do
        let i'j's = map cph hs
            pns = map (toList.scale 0.25.fst.fromComplex) i'j's
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex pns)

-- | image of the circular points under by the homography of a metric plane
     
cph :: Mat -> Vector (Complex Double)
cph h = ihc (h1 + i `scale` h2)
  where [h1,h2,_] = toColumns (complex h)

-- | convert to inhomogenous complex vector
ihc :: Vector (Complex Double) -> Vector (Complex Double)
ihc v = scale (recip w) (subVector 0 (n-1) v)
  where n = dim v
        w = v@>(n-1)

----------------------------------------------------------------------

letters :: IO [(Polyline,String)]
letters = flip zip (reverse $ map return "ABCDEFGHIJKLMNOPQRSTUVWXYZ") . map fst . readt <$> readFile "letters.txt"
    where readt x = read x :: [(Polyline,String)]

----------------------------------------------------------------------

digits = digits' "digits.jpg"

digits' :: FilePath -> IO (Sample Polyline)
digits' path = do
    cam <- mplayer ("mf://"++path) (mpSize 40)
    img <- (grayscale . channels) `fmap` cam
    let rawconts = contours 100 1 128 False img
        fst3 (a,_,_) = a
        proc = Closed . pixelsToPoints (size img).douglasPeuckerClosed 1 .fst3
        cs = (map proc $ rawconts) `zip` cycle ["$","0","1","2","3","4","5","6","7","8","9"]
    return cs

----------------------------------------------------------------------

auxMoments (s,sx,sy,sx2,sy2,sxy,sx3,sx2y,sxy2,sy3) seg@(Segment (Point a b) (Point c d))
    = (s   + (a*d-c*b)/2,
       sx  + ( 2*a*c*(d-b)-c^2*(2*b+d)+a^2*(2*d+b))/12,
       sy  + (-2*b*d*(c-a)+d^2*(2*a+c)-b^2*(2*c+a))/12,
       sx2 + ( (a^2*c+a*c^2)*(d-b) + (a^3-c^3)*(b+d))/12,
       sy2 + (-(b^2*d+b*d^2)*(c-a) - (b^3-d^3)*(a+c))/12,
       sxy + ((a*d-c*b)*(a*(2*b+d)+c*(b+2*d)))/24,
       sx3 + (2*a**3*c*(b-d) + 2*a**2*c**2*(b-d) + 2*a*c**3*(b-d) - a**4*(3*b + 2*d) + c**4*(2*b + 3*d))/40,
       sx2y + (a**2*c*(b - d)*(3*b + 2*d) + a*c**2*(b - d)*(2*b + 3*d) - a**3*(b**2 + 3*b*d + d**2) + c**3*(b**2 + 3*b*d + d**2))/60,
       sxy2 + (b**3*(a**2 + 3*a*c + c**2) + b**2*(-3*a**2 + a*c + 2*c**2)*d - b*(a - c)*(2*a + 3*c)*d**2 - (a**2 + 3*a*c + c**2)*d**3)/60,
       sy3 + (b**4*(3*a + 2*c) + 2*b**3*(-a + c)*d + 2*b**2*(-a + c)*d**2 + 2*b*(-a + c)*d**3 - (2*a + 3*c)*d**4)/40)

rawMoments = foldl' auxMoments (0,0,0,0,0,0,0,0,0,0). asSegments

data Moments = Moments {
    m_a :: Double,
    m_mx, m_my :: Double,
    m_cxx, m_cyy, m_cxy :: Double,
    m_cxxx, m_cxxy, m_cxyy, m_cyyy :: Double }
    

moments l = Moments {..}
  where
    (s,sx,sy,sx2,sy2,sxy,sx3,sx2y,sxy2,sy3) = rawMoments l
    m_a = s
    m_mx = sx/s
    m_my = sy/s
    m_cxx = sx2/s - m_mx*m_mx
    m_cyy = sy2/s - m_my*m_my
    m_cxy = sxy/s - m_mx*m_my
    m_cxxx = 0
    m_cxxy = 0
    m_cxyy = 0
    m_cyyy = 0

