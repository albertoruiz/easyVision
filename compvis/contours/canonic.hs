{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

import EasyVision as EV
import Graphics.UI.GLUT hiding (Point)
import Util.Misc(diagl,degree,Vec,norm,debug,diagl,memo,Mat,mat,norm)
import Util.Rotation(rot3)
import Control.Arrow
import Control.Applicative((<$>),(<*>))
import Control.Monad(when,ap)
import Data.Colour.Names as Col
import Vision(desp, estimateHomography)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4)
import Data.Function(on)
import Text.Printf(printf)
import Data.IORef
import Util.Options

import Data.Complex
import Classifier(Sample)

import Tools

$(autoParam "SCParam" "classify-"
  [( "epsilon","Double" ,realParam 0.2 0 1),
   ( "conf"   ,"Double" ,realParam 1.5 1 3)]
 )

mon = monitor "Image pauser" (mpSize 10) (drawImage . EV.gray)


--recognize = shapeCatalog (return [])
--recognize = shapeCatalog pentos
--recognize = shapeCatalog digits

catalog = digits
--catalog = return pentominos
--catalog = read <$> readFile "digits.txt"

main = run $   camera ~> EV.gray
           >>= wcontours id ~> (id *** contSel)
           >>= shapeCatalog catalog (protos . protoOri)
           >>= recognize classify
           >>= timeMonitor

main' = run $ (camera ~> EV.gray
            >>= wcontours id ~> (id *** contSel)
            >>= shapeCatalog catalog (protos . protoOri)
            .&. winSCParam) ~> pureClassify >>= classifyMon
            >>= timeMonitor


recognize meth cam = do
    par <- winSCParam
    monitor "contours" (mpSize 20) (sh par) cam
  where
    sh par (im, cs, prots) = do
        drawImage' im
        pointCoordinates (size im)
        text2D 0.9 0.6 $ show (length cs)
        p <- par
        when (not (null prots)) $ mapM_ (meth p prots) cs


chPol (Closed ps) = Closed (convexHull ps)

feat = map foufeat . icaConts' . whitenContour

alignRefined SCParam {..} prots x = do
    let (zs,c) = (toCanonic &&& id) x
        (d,(mb,(cx,cb))) = minimumBy (compare `on` fst)  [(dist u v, (m,(u,v))) | (m,u) <- zs, (v,_) <- prots]
        af1 = inv mb
        al = transPol af1 (invFou 100 10 $ toFun cb)
        ob = transPol af1 (invFou 100 10 $ toFun cx)
        corr = zipWith (\a b->[a,b]) (polyPts al) (polyPts ob)
        
        aux (Closed ps) = map p2l ps
        p2l (Point x y) = [x,y]
        
        h = estimateHomography (aux al) (aux ob)
        alp = transPol h x
        
        (zs',c') = (toCanonic &&& id) alp
        (d',(mb',cx')) = minimumBy (compare `on` fst)  [(dist u cb, (m,u)) | (m,u) <- zs']
        al' = transPol (inv h <> inv mb') (invFou 100 10 $ toFun cb)
        
    lineWidth $= 1

    setColor' green
    shcont al
    lineWidth $= 1
    setColor' Col.gray
    renderPrimitive Lines $ mapM_ vertex (concat corr)
        
    setColor' orange
    shcont alp
    when (d' < epsilon) $ do
        lineWidth $= 1
        setColor' yellow
        shcont al'

    setColor' red
    lineWidth $= 1
    shcont x
    shcont ob



align SCParam {..} prots x = do
--    shcont x
    let (zs,c) = (toCanonic &&& id) x
    let (d,(mb,(cx,cb))) = minimumBy (compare `on` fst)  [(dist u v, (m,(u,v))) | (m,u) <- zs, (v,_) <- prots]
    when (not (null prots) && d < epsilon) $ do
        lineWidth $= 1
        let al = transPol (inv mb) (invFou 100 10 $ toFun cb)
            ob = transPol (inv mb) (invFou 100 10 $ toFun cx)
            corr = zipWith (\a b->[a,b]) (polyPts al) (polyPts ob)
        setColor' Col.gray
        let aux (Closed ps) = map p2l ps
            p2l (Point x y) = [x,y]
        renderPrimitive Lines $ mapM_ vertex (concat corr)
        lineWidth $= 1
        setColor' yellow
        shcont al
--    lineWidth $= 1
--    setColor' red
--    shcont x

classify SCParam {..} prots x = do
--    setColor' red
--    lineWidth $= 1
--    shcont x
    let (zs,c) = (feat &&& id) x
    let prep = take 3
             . sortBy (compare `on` fst)
             . map ((minimum *** head) . unzip)
             . groupBy ((==) `on` snd)
             . sortBy (compare `on` snd)
    let lb = prep $ map (clas zs &&& snd) prots
    setColor' blue
    let (ox,oy,_,_,_) = momentsContour (polyPts c)
    let up = 2/400 + maximum (map py (polyPts c))
    let (d1,l):(d2,_):_ = lb
    when (d1 < epsilon && d2 > conf*d1) $ do
        text2D (d2f ox) (d2f up) l
        lineWidth $= 1
        shcont c

-- input: the result of shapeCatalog: image, contours, prototypes and classification parameters
-- output: image and well classified shapes
pureClassify ((im,cs,prots),SCParam {..}) = (im, oks)
  where
    oks = map (snd) $ filter ((<epsilon).fst) (map clas cs) 
    clas x = minimumBy (compare `on` fst)  [(dist u v, (x,l)) | (m,u) <- zs, (v,l) <- prots]
      where zs = toCanonic x

classifyMon cam = monitor "Detected" (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        text2D 0.9 0.6 $ show (length oks)
        mapM_ f oks
    f (c,l) = do
        shcont c
        text2D (d2f ox) (d2f up) l
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)
        up = 2/400 + maximum (map py (polyPts c))
        

-- with more detail
classify' prots x = do
    let (zs,c) = (feat &&& id) x
    let prep = take 3
             . sortBy (compare `on` fst)
             . map ((minimum *** head) . unzip)
             . groupBy ((==) `on` snd)
             . sortBy (compare `on` snd)
    let lb = prep $ map (clas zs &&& snd) prots
    setColor' red
    let (ox,oy,_,_,_) = momentsContour (polyPts c)
    when (fst(head lb) < 0.3) $ do
        let (d1,l):(d2,_):_ = lb
        let s = if d2 < 1.5*d1 
                  then concatMap (\(d,l') -> printf "%s %.0f  " l' (100*d)) lb
                  else l
        text2D (d2f ox) (d2f oy) s
    --mapM_ (print.clas zs) protos
    

dist a b = norm (a-b)

clas zs p = minimum $ map (dist (fst p)) zs

d2f = fromRational.toRational

----------------------------------------------------------------------

-- DEMO > smooth
-- show frequential smoothing of the equalized shapes

smooth = run $ camera ~> EV.gray >>= wcontours id ~> (id *** contSel) >>= smoothTest >>= timeMonitor

smoothTest = monitor "contours" (mpSize 20) sh where
    sh (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        lineWidth $= 1
        pointSize $= 3
        setColor' orange
        mapM_ (shcont) cs
        setColor' red
        mapM_ (shcont.dispfeat) cs
        --mapM_ (print.orientation) cs

    dispfeat = invFou 100 10 . normalizeStart . memo 20 . fourierPL . equalizeContour

----------------------------------------------------------------------

-- DEMO > orient
-- show canonic orientation of detected shapes

orient = run $ camera ~> EV.gray >>= wcontours id ~> (id *** contSel) >>= monitorOrient >>= timeMonitor

monitorOrient = monitor "contours" (mpSize 20) sh where
    sh (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' orange
        lineWidth $= 1
        mapM_ shcont cs
        let wcs1 = map (head . f) cs
            wcs2 = map (last . f) cs
        setColor' red
        lineWidth $= 2
        mapM_ shcont wcs1
        setColor' blue
        lineWidth $= 2
        mapM_ shcont wcs2

    f c@(Closed ps) = map (transPol t) . icaConts . whitenContour $ c
      where
        t = desp (ox,oy) <> diagl [0.05,0.05,1]
        (ox,oy,_,_,_) = momentsContour ps

----------------------------------------------------------------------


-- DEMO > showCanonic
-- show canonic versions of shapes and other interesting info


runIt f = prepare >> f >> mainLoop

showpentoR = examplesBrowser "pentominos" (EV.Size 500 500) f pentominos
  where
    f = shcontO . transPol (diagl[0.1,0.1,1])

showpentoW = examplesBrowser "pentominos" (EV.Size 500 500) f (protoOri pentominos)
  where
    f = shcontO . transPol (diagl[0.1,0.1,1])

showpentoF = examplesBrowser "pentominos" (EV.Size 500 500) f pentominos
  where
    f c = do
        let x = invFou 100 5 . memo 20 . fourierPL $ c
        setColor' white
        shcontO . transPol (diagl[0.1,0.1,1]) $ c
        setColor' yellow
        shcontO . transPol (diagl[0.1,0.1,1]) $ x        


showpentoCH = examplesBrowser "Convex Hull" (EV.Size 500 500) f pentominos
  where
    f c = do
        let x = chPol $ transPol (diagl[0.1,0.1,1]) $ c
        setColor' white
        shcontO . transPol (diagl[0.1,0.1,1]) $ c
        setColor' yellow
        shcontO x
        setColor' blue
        pointSize $= 2
        renderPrimitive Points $ mapM_ vertex (polyPts x)

showDigits = do
    ds <- digits
    examplesBrowser "digits" (EV.Size 500 500) f ds
  where
    f = shcontO . transPol (diagl[0.1,0.1,1]) . whitenContour


showCanonic = runIt $ showpentoR
                   >> showpentoW
                   >> showpentoF
                   >> showpentoCH
                   >> showDigits

----------------------------------------------------------------------

pentos :: IO (Sample Polyline)
pentos = return pentominos

digits :: IO (Sample Polyline)
digits = do
    cam <- mplayer "mf://digits.jpg" (mpSize 40)
    img <- (EV.gray. channels) `fmap` cam
    let rawconts = contours 100 1 128 False img
        fst3 (a,_,_) = a
        proc = Closed . pixelsToPoints (size img).douglasPeuckerClosed 1 .fst3
        cs = (map proc $ rawconts) `zip` cycle ["$","0","1","2","3","4","5","6","7","8","9"]
    return cs

----------------------------------------------------------------------

-- DEMO > analyze pentos
-- show discrimination matrix for different distances in a given problem

analyzeAlign shapes = r
  where
    xs = map (map snd . toCanonic . fst) shapes
    r = mat [ [ distMult a b | b <-xs ] | a <- [0]:xs ]
    distMult us vs = minimum $ dist <$> us <*> vs


analyzeInvar f shapes = r
  where
    xs = map (f.fst) shapes
    r = mat [ [ norm (a-b) | b <-xs ] | a <- 0:xs ]


oldInvar w f = fromList desc where
    sc = 1 -- g 1
    g k = magnitude (f k) + magnitude (f (-k))
    h k = g k/sc
    desc = map h [2..(max 2 w)]

simpleInvar w f = fromList desc where
    g = magnitude . f
    desc = map g [-w .. w]



disp = putStr . dispf 0 . (100*)

analyze gen = do
    shapes <- fmap (filter ((not.(`elem`["9","Y'","J","S","B","Q","N'"])).snd)) gen
    let a = analyzeInvar (simpleInvar 10 . fourierPL . whitenContour) shapes
        b = analyzeAlign shapes
        c = analyzeInvar (oldInvar 10 . fourierPL . whitenContour) shapes
    dspnor c
    dspnor a
    dspnor b
    dspnor $ (b-a)/b

dspnor m = do
    putStr . dispf 0 . (*100) $ m             

----------------------------------------------------------------------

