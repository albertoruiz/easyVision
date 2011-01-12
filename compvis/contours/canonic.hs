import EasyVision as EV hiding (debug)
import Graphics.UI.GLUT hiding (Point)
import Util.Misc(diagl,degree,Vec,norm,debug,diagl,memo)
import Util.Rotation(rot3)
import Control.Arrow
import Data.Colour.Names as Col
import Vision(desp)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(minimumBy,sort)
import Data.Function(on)

import Data.Complex
import Classifier(Sample)


main = run $ camera ~> EV.gray >>= wcontours ~> (id *** contSel) >>= monitorC >>= timeMonitor

orient = run $ camera ~> EV.gray >>= wcontours ~> (id *** contSel) >>= monitorOrient >>= timeMonitor

test = run $ camera ~> EV.gray >>= wcontours ~> (id *** contSel) >>= monitorTest >>= timeMonitor

monitorC = monitor "contours" (mpSize 20) sh where
    sh (im, cs) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' orange
        lineWidth $= 1
        mapM_ shcont cs
        let xs = map (feat &&& id) cs
        --print $ concatMap (map dim . fst) xs
        text2D 0.9 0.6 $ show (length xs)
        mapM_ classify xs
        --putStrLn "----------------------------"

feat = map (prec 10 .  normalizeStart . memo 20 . fourierPL) . icaConts' . whitenContour

classify (zs,c) = do
    let (ox,oy,_,_,_) = momentsContour (polyPts c)
    let lb = snd $ minimumBy (compare `on` clas zs) protos
    setColor' red
    text2D (d2f ox) (d2f oy) lb
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

protoOri :: Sample Polyline
protoOri = concatMap (repSample . (f *** id)) pentominos
  where
    f = icaConts'' . whitenContour . transPol (rot3 (10*degree)) -- hmm
    repSample (xs,l) = map (id &&& const l) xs


protos :: [(Vec,String)]
protos = map (g *** id) protoOri
  where
    g = prec 10 .  normalizeStart . memo 20 . fourierPL


prec n f = join[r,c]
    where 
      (r,c) = fromComplex $ fromList $ map f ([1 .. n]++[-1,-2 .. -n])

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
    acts = [((MouseButton WheelUp,   Down, modif), \(k,exs) -> (min (k+1) n, exs))
           ,((MouseButton WheelDown, Down, modif), \(k,exs) -> (max (k-1) 0, exs))]

runIt f = prepare >> f >> mainLoop

showpentoR = examplesBrowser "pentominos" (EV.Size 500 500) f pentominos
  where
    f = shcontO . transPol (diagl[0.1,0.1,1])

showpentoW = examplesBrowser "pentominos" (EV.Size 500 500) f protoOri
  where
    f = shcontO . transPol (diagl[0.1,0.1,1])

showpentos = runIt $ showpentoR
                  >> showpentoW

