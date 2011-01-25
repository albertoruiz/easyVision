{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

import EasyVision as EV hiding (debug)
import Graphics.UI.GLUT hiding (Point)
import Util.Misc(diagl,degree,Vec,norm,debug,diagl,memo,Mat)
import Util.Rotation(rot3)
import Control.Arrow
import Control.Applicative((<$>))
import Control.Monad(when)
import Data.Colour.Names as Col
import Vision(desp)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4)
import Data.Function(on)
import Text.Printf(printf)

import Data.Complex
import Classifier(Sample)

$(autoParam "SCParam" "classify-"
  [( "epsilon","Double" ,realParam 0.2 0 1),
   ( "conf"   ,"Double" ,realParam 1.5 1 3)]
 )

--recognize = monitorC pentos
recognize = monitorC digits

main = run $ camera ~> EV.gray >>= wcontours id ~> (id *** contSel) >>= recognize >>= timeMonitor

orient = run $ camera ~> EV.gray >>= wcontours id ~> (id *** contSel) >>= monitorOrient >>= timeMonitor

test = run $ camera ~> EV.gray >>= wcontours id ~> (id *** contSel) >>= monitorTest >>= timeMonitor

monitorC gprot cam = do
    par <- winSCParam
    prototypes <- (protos . protoOri) <$> gprot
    monitor "contours" (mpSize 20) (sh par prototypes) cam
  where
    sh par prots (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' red
        lineWidth $= 1
        mapM_ (shcont) cs
        text2D 0.9 0.6 $ show (length cs)
        p <- par
        mapM_ (align p prots) cs

chPol (Closed ps) = Closed (convexHull ps)

feat = map foufeat . icaConts' . whitenContour



align SCParam {..} prots x = do
    let (zs,c) = (toCanonic &&& id) x
    let (d,(mb,cb)) = minimumBy (compare `on` fst)  [(dist u v, (m,v)) | (m,u) <- zs, (v,_) <- prots]
    when (d < epsilon) $ do
        lineWidth $= 3
        setColor' green
        let al = transPol (inv mb) (invFou 100 10 $ toFun cb)
        shcont al


classify SCParam {..} prots x = do
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
        lineWidth $= 4
        shcont c

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

monitorTest = monitor "contours" (mpSize 20) sh where
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

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
    renderPrimitive Points $ vertex (head c)
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

-- | smoothed frequential invariant representations
toCanonic :: Polyline -> [(Mat,Vec)]
toCanonic p = zip (map (<>w) cs) cps
  where
    w = rot3 (20*degree) <> whitener p
    wp = transPol w p
    cs = map rot3 (icaAngles' wp)
    cps = map (foufeat .flip transPol wp) cs

protoOri :: Sample Polyline -> Sample Polyline
protoOri = concatMap (repSample . (f *** id))
  where
    f = icaConts'' . whitenContour . transPol (rot3 (10*degree)) -- hmm
    repSample (xs,l) = map (id &&& const l) xs


protos ::  Sample Polyline -> Sample Vec
protos = map (foufeat *** id)

foufeat = prec 10 .  normalizeStart . fourierPL


prec' n f = join[r,c]
    where 
      (r,c) = fromComplex $ fromList $ map f ([1 .. n]++[-1,-2 .. -n])

prec n f = join[r,c]
    where 
      (r,c) = fromComplex $ fromList $ map f ([-n..n])


toFun cb k = cb@>(k+n) :+ cb@>(k+3*n+1)
    where n = (dim cb - 2) `div` 4


----------------------------------------------------------------------

icaConts w = map g (icaAngles w)
  where
    g a = transPol (rot3 a) w

icaConts' w = map g (icaAngles' w)
  where
    g a = transPol (rot3 a) w

icaConts'' w = map g (icaAngles'' w)
  where
    g a = transPol (rot3 a) w

icaAngles' w = as ++ map (+pi) as
  where as = icaAngles w

icaAngles'' w = [head as, last as]
  where as = icaAngles w

----------------------------------------------------------------------

examplesBrowser :: String -> EV.Size -> (t -> IO a) -> Sample t -> IO (EVWindow (Int, Sample t))
examplesBrowser name sz f exs =
    evWindow (0,exs) name sz (Just disp) (mouseGen acts kbdQuit)
  where
    n = length exs - 1
    disp st = do
        (k,exs) <- get st
        let (x,label) = exs!!k
        f x
        windowTitle $= name++" #"++show (k+1)++ ": "++label
    acts = [((MouseButton WheelUp,   Down, modif), \_ (k,exs) -> (min (k+1) n, exs))
           ,((MouseButton WheelDown, Down, modif), \_ (k,exs) -> (max (k-1) 0, exs))]

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


showpentos = runIt $ showpentoR
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
        cs = (map proc $ rawconts) `zip` cycle ["$","0","1","2","3","4","5","6","7","8","6"]
    return cs

