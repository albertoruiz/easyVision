module Tools(
    -- * Invariant features
    toCanonicAll, toCanonic2,
--    protos, 
    protoOri,
--    foufeat, 
    toFun, 
--    prec,
    icaConts,
--  icaContsAll, icaConts2,
    alignment, refine,
    -- * Projective rectification from circles
    rectifyMon, rectifierFromManyCircles, imagOfCircPt,
    -- * Digit tools
    fixOrientation,
    -- * Display
    shapeCatalog,
    examplesBrowser,
    shcont, shcontO,
    rotAround
) where


import EasyVision as EV
import Graphics.UI.GLUT hiding (Point)
import Util.Misc(diagl,degree,Vec,norm,debug,diagl,memo,Mat,mat,norm,unionSort,replaceAt,mean,median,unitary,pairsWith)
import Util.Covariance(meanV,covStr)
import Util.Rotation(rot3)
import Control.Arrow
import Control.Applicative((<$>),(<*>))
import Control.Monad(when,ap)
import Data.Colour.Names as Col
import Vision (desp, scaling, estimateHomography, ht, cross,
               inHomog,circularConsistency,focalFromCircularPoint,similarFrom2Points,
               rectifierFromCircularPoint)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4,partition,transpose)
import Data.Function(on)
import Text.Printf(printf)
import Data.IORef
import Util.Options
import Util.Ellipses

import Data.Complex
import Classifier(Sample)
import Data.Maybe(isJust,catMaybes)
import Numeric.GSL.Minimization

----------------------------------------------------------------------
----------------------------------------------------------------------

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
    ufr ang = prec 10 $ normalizeStart $ (cis ang *) . uf
    angs = map (* degree) [-amax .. amax]
    d' = dist (ufr alpha) v
    u' = ufr alpha
    b' = rot3 (-alpha) <> b

dist a b = norm (a-b)
    
----------------------------------------------------------------------    
    
fixOrientation :: (a, [AlignInfo]) -> (a, [AlignInfo])
fixOrientation (im, xs) = (im, zs ++ ys)
  where
    asym (_, (_,_,_,_,_,l)) = l `elem` ["1","2","3","4","5","7"]
    (zs, qs)  = partition asym xs
    ys = if null zs then qs else map (f dx dy) qs
    g (_, (_,b,_,a,_,_)) = dir $ ht (inv b <> a) [[0,0],[1,0]]
    dir [[ox,oy],[x,y]] = [x-ox,y-oy]
    [dx,dy] = map mean $ transpose (map g zs)

    f dx dy (d, (x,b,u,a,v,l)) | l `elem` ["$","6","9","8"] && flipped = (d, (x,b'',u,a,v, comp l))
                               | l == "0" = (d, (x,b',u,a,v,l))
                               | otherwise = (d, (x,b,u,a,v,l))
      where
        [[cx,cy],diry ] = ht (inv b <> a) [[0,0],[1,0]]
        [r,s] = dir [[cx,cy],diry]
        dot = r*dx + s*dy
        flipped = dot < 0
        b'' = rot3 pi <> b
        aangle = acos (dot/(sqrt(dx*dx+dy*dy)*sqrt(r*r+s*s)))
        [[tenx,teny]] = ht (rot3 (aangle)) [[r,s]]
        angle | tenx * dx + teny * dy > 0 = aangle
              | otherwise = -aangle
        b' = b <> desp (cx,cy) <> rot3 (-angle) <> desp (-cx,-cy)
        comp "6" = "9"
        comp "9" = "6"
        comp x = x
    
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



protoOri :: Sample Polyline -> Sample Polyline
protoOri = concatMap (repSample . (f *** id))
  where
    f = icaConts2 . whitenContour . transPol (rot3 (10*degree)) -- hmm
    repSample (xs,l) = map (id &&& const l) xs


protos ::  Sample Polyline -> Sample Vec
protos = map (foufeat *** id)


foufeat = prec 10 .  normalizeStart . fourierPL


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

shapeCatalog gprot feat' cam = do
    raw <- gprot
    let feat = feat' . map (normalShape *** id)
        prototypes = feat raw
        sz = mpSize 10
    w <- evWindow (raw,prototypes,Nothing,False) "Contour Selection" sz Nothing (marker kbdQuit)
    s <- shapesBrowser "Shape" (EV.Size 240 240) raw
    return $ do
        (raw',prots',click,save) <- getW w
        (im,cs) <- cam
        let (raw,prots) = case click of
                Nothing -> (raw',prots')
                Just pix  -> let [pt] = pixelsToPoints sz [pix]
                                 newc = (closestTo pt cs, "?" ++ show (length prots' + 1))
                           in (newc:raw', feat [newc] ++ prots')
        when (isJust click) $ putW s (0, raw) >> postRedisplay (Just (evW s))
        inWin w $ do
            drawImage im
            pointCoordinates sz
            text2D 0.9 0.6 $ show (length cs)
            mapM_ shcont cs
            
        when save $ writeFile "shapes.txt" (show raw)
        putW w (raw,prots,Nothing,False)
        return (im,cs,prots)
  where
    marker _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
        (r,p,_,_) <- readIORef st
        let clicked = Pixel (fromIntegral y) (fromIntegral x)
        writeIORef st (r,p, Just clicked, False)

    marker _ st (Char 's') Down _ _ = do
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


examplesBrowser :: String -> EV.Size -> (t -> IO a) -> Sample t -> IO (EVWindow (Int, Sample t))
examplesBrowser name sz f exs =
    evWindow (0,exs) name sz (Just disp) (mouseGen acts kbdQuit)
  where
    disp st = do
        pointCoordinates sz
        (k,exs) <- get st
        when (not $ null exs) $ do 
            let (x,label) = exs!!k
            f x
            windowTitle $= name++" #"++show (k+1)++ ": "++label
    acts = [((MouseButton WheelUp,   Down, modif), \_ (k,exs) -> (min (k+1) (length exs - 1), exs))
           ,((MouseButton WheelDown, Down, modif), \_ (k,exs) -> (max (k-1) 0, exs))]

----------------------------------------------------------------------

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
--    renderPrimitive Points $ vertex (head c)
shcont (Open c) = do
    renderPrimitive LineStrip $ mapM_ vertex c

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
  
----------------------------------------------------------------------

-- | obtain a rectifing homograpy from several conics which are the image of circles 
--rectifierFromManyCircles :: [Mat] -> Maybe Mat
rectifierFromManyCircles f cs = r ijs
  where
    cqs = zip cs (map (fst.analyzeEllipse) cs)
    ijs = catMaybes (pairsWith imagOfCircPt cqs)
    r [] = ident 3
    r xs = rectifierFromCircularPoint (f $ average' xs)
    average xs = (x,y) where [x,y] = toList (head xs)
    average' zs = (x,y)
      where
        pn = meanV . covStr . fromRows $ map (fst.fromComplex) zs
        cpn = complex pn
        ds = Util.Misc.median $ [pnorm PNorm2 (z - cpn) | z <- zs]
        [pnx,pny] = toList pn
        dir = scalar i * complex (scalar ds * unitary (fromList [pny, -pnx]))
        [x,y] = debug "I'" id $ toList (cpn + dir)


imagOfCircPt :: (Mat,EllipseParams) -> (Mat,EllipseParams) -> Maybe (Vector (Complex Double))
imagOfCircPt (c1,q1) (c2,q2) = (fmap h.fst) (selectSol q1 q2 (intersectionEllipses c1 c2))
  where
    h (a,b) = fromList [a,b]      

improveCirc (rx:+ix,ry:+iy) ells = (rx':+ix',ry':+iy') where
    [rx',ix',ry',iy'] = fst $ minimize NMSimplex2 1e-5 300 (replicate 4 0.1) cost [rx,ix,ry,iy]
    cost [rx,ix,ry,iy] = sum $ map (eccentricity.rectif) $ ells
        where rectif e = mt t <> e <> inv t
              t = rectifierFromCircularPoint (rx:+ix,ry:+iy)
    eccentricity con = d1-d2 where (_,_,d1,d2,_) = fst $ analyzeEllipse con

mt m = trans (inv m)
----------------------------------------------------------------------

rectifyMon sz cam = do wr <- evWindow () "Rectif"   sz Nothing (const kbdQuit)
                       monitor "Rectify" (mpSize 10) (sh wr) cam
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
 
    sh wr (im,oks) = when (length oks > 2) $ do
        drawImage im
        pointCoordinates (mpSize 10)
        mapM_ (f.g) oks        
        let gc (d, (x,b,u,a,v,l)) = x
            ellipMat = map (h.g) (sortBy (compare `on` fst) oks)
            ellipses = map (fst.analyzeEllipse) ellipMat
            improve = id
            sc = 1
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
                shLine h1
                setColor' Col.yellow
                shLine h2
                pointSize $= 5
                setColor' Col.purple
                renderPrimitive Points $ mapM (vertex.map realPart.t2l) [ij,other]
                setColor' Col.green
                mapM_ shLine $ map (map realPart) $ tangentEllipses (ellipMat!!0) (ellipMat!!1)
                
                let cc = inv (ellipMat!!0) <> (ellipMat!!1)
                    vs = map (fst . fromComplex) . toColumns . snd . eig $ cc
                setColor' Col.white
                renderPrimitive Points (mapM (vertex . toList. inHomog) vs)

                let ccl = (ellipMat!!0) <> inv (ellipMat!!1)
                    vsl = map (fst . fromComplex) . toColumns . snd . eig $ ccl
                mapM_ (shLine.toList) vsl



                let recraw = rectifierFromCircularPoint ij
                             --rectifierFromManyCircles id (ellipMat)
                    (mx,my,_,_,_) = head ellipses
                    (mx2,my2,_,_,_) = last ellipses
                    [[mx',my'],[mx'2,my'2]] = ht recraw [[mx,my],[mx2,my2]]
                    -- okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [0,0] [-0.5, 0] <> recraw
                    okrec = similarFrom2Points [mx',my'] [mx'2,my'2] [mx,my] [mx2, my2] <> recraw
                
                    --drawImage $ warp 0 sz' (scaling sc <> w) (gray orig)
                inWin wr $ drawImage $ warp 0 sz (scaling sc <> okrec ) ( orig)
                --text2D 30 30 $ show $ focalFromHomogZ0 (inv recraw)
                -- it is also encoded in the circular points
                text2D 30 30 $ printf "f = %.2f" $ focalFromCircularPoint ij
                text2D 30 50 $ printf "ang = %.1f" $ abs ((acos $ circularConsistency ij)/degree - 90)

unitCircle = Closed [Point (cos (t*degree)) (sin (t*degree)) | t <- [0,10 .. 350]]


getHorizs pts = map linf pts where
    linf (x,y) = toList $ unitary $ snd $ fromComplex $ cross v (conj v)
        where v = fromList [x,y,1]

selectSol (x1,y1,_,_,_) (x2,y2,_,_,_) pts = (ij,other) where
    ls = getHorizs pts
    ij    = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) > 0 ]
    other = mbhead [v | (v,l) <- zip pts ls, f l (x1,y1) * f l (x2,y2) < 0 ]
    f [a,b,c] (x,y) = a*x + b*y + c
    mbhead [] = Nothing
    mbhead x  = Just (head x)

tangentEllipses c1 c2 = map ((++[1]). t2l) $ intersectionEllipses (inv c1) (inv c2)

t2l (a,b) = [a,b]

shLine [a,b,c] = renderPrimitive Lines $ mapM f [-1,1]
    where f x = vertex $ Point x ((-a*x-c)/b)


