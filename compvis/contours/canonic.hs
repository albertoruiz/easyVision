import EasyVision as EV
import Graphics.UI.GLUT hiding (Point,Size)
import Util.Misc(diagl,degree,vec,Vec,norm,debug,diagl,memo,Mat,mat,norm)
import Util.Rotation(rot3)
import Control.Arrow
import Control.Applicative((<$>),(<*>))
import Control.Monad(when,ap)
import Data.Colour.Names as Col
import Vision(desp, cross, estimateHomography,cameraFromHomogZ0, homog, inHomog,
              cameraFromAffineHomogZ0,factorizeCamera,ht,cameraAtOrigin,
              similarFrom2Points, scaling)
import Numeric.LinearAlgebra -- ((<>),fromList,toList,fromComplex,join,dim)
import Data.List(sortBy,minimumBy,groupBy,sort,zipWith4,transpose)
import Data.Function(on)
import Text.Printf(printf)
import Data.IORef
import Util.Options
import Util.Ellipses
import ImagProc(otsuThreshold)


import Data.Complex
import Classifier(Sample)

import Tools


--catalog = digits
catalog = pentos
--catalog = read <$> readFile "digits.txt"
--catalog = letters

main = main1

main2 = run $ camera ~> EV.gray
            >>= wcontours id ~> (id *** contSel)
            >>= shapeCatalog normalShape catalog (concatMap toCanonic2)
       

main1 = run $ camera ~> EV.gray
            >>= wcontours id ~> (id *** contSel)
            >>= shapeCatalog normalShape catalog (concatMap toCanonic2)
            >>= classifier ~> snd
--            >>= classifyMon
            >>= alignMon' "Alignment"
--            >>= parseMon
            >>= rectifyMon (mpSize 10)
--            ~> tryMetric
--            >>= alignMon
--            >>= virtualMon
            >>= virtualExperiment axes
            >>= virtualMonAffine vertical
            >>= virtualMonAffine elevated
            >>= timeMonitor

main3 = run $ camera ~> EV.gray
            >>= wcontours id ~> (id *** contSel)
            >>= shapeCatalog centerShape catalog (concatMap toCanonic2)
            >>= classifier ~> snd
            >>= alignMon' "Pre"
            >>= rectifyMon (mpSize 10)
            ~> tryMetric
            >>= alignMon' "Post"
            >>= virtualMon
            >>= timeMonitor


----------------------------------------------------------------------
type OKS = (Double, (Polyline,Mat,Vec,Mat,Vec,String))

virtualMon = monitor "Virtual" (mpSize 20) sh
  where
    f (_, (x,_,_,_,_,_)) = x
    g (_, (_,b,_,a,_,_)) = c where Just c = cameraFromHomogZ0 (Just 1.7) (inv b <> a <> diagl[1,-1,1])
    h cam = do
        cameraView cam (4/3) 0.1 100
        unitCube -- houseModel
        setColor' purple
        lineWidth $= 3
        renderPrimitive LineLoop $ mapM_ vertex $ [[0,0,0],
                                                   [0,0,2::Double],
                                                   [0,0,0],[2,0,0],[0,0,0],[0,2,0]]
        
        
    sh (im,oks) = when (length oks > 2) $ do
        clear [DepthBuffer]
        drawImage (im::ImageGray)
        clear [DepthBuffer]
        --pointCoordinates (mpSize 10)
        depthFunc $= Just Less
        
        mapM_ (h.g) (oks::[OKS])

----------------------------------------------------------------------

virtualExperiment h cam = do wr <- evWindow () "Rectify Vertical"  (mpSize 10) Nothing (const kbdQuit)
                             monitor "Virtual Affine" (mpSize 20) (sh wr) cam
  where
    f (_, (x,_,_,_,_,_)) = x
    g (_, (x,b,_,a,_,l)) = (c,x,h)
      where 
        h = inv b <> a
        c = cameraFromAffineHomogZ0 1.7 (h <> diagl[-1,1,1])
    s (_, (_,_,_,_,_,l)) = not (l `elem` ["I","O","0"])
        
    sh wr (im,oks) = do
        clear [DepthBuffer]
        drawImage (im::ImageGray)
        clear [DepthBuffer]
        pointCoordinates (mpSize 10)
        depthFunc $= Just Less
        let rec = map g (filter s oks::[OKS])
        
        mapM_ h rec
        r <- h2 rec
        inWin wr $ drawImage' $ warp 0 (Size 400 400) r im
        return ()

h2 oks = do
    let f(c,_,_) = homog (vec a) `cross ` homog (vec b)
          where [a,b] = ht c [[0,0,0],[0,0,5::Double]]
        v = intersectionManyLines $ map f oks
    --print (inHomog v)
    pointSize $= 5
    setColor' red
    let ihv = toList $ inHomog v
    renderPrimitive Points (vertex ihv)
    let f = 1.7
        w = diagl [1,1,f**2]
        h = w<>v
        r = rectifierFromHorizon f h
        area (_,x,_) = abs (orientation x)
        center (_,z,_) = [x,y]
           where (x,y,_,_,_) = momentsContour (polyPts z)
        [c1,c2] = take 2 $ map center $ sortBy (compare `on` negate . area) oks
        [a,b] = ht r [c1,c2]
        r' = if length oks > 1 then scaling 0.5 <> similarFrom2Points a b c1 c2 <> r
                               else ident 3
    shLine (toList h)
    setColor' Col.gray
    lineWidth $= 1
    let ff (c,_,_) = [ihv, head (ht c [[0,0,0]])]
    renderPrimitive Lines $ mapM_ vertex (concatMap ff oks) 
    
    return r'
--    putStrLn "----------------------"

----------------------------------------------------------------------

virtualMonAffine h = monitor "Virtual Affine" (mpSize 20) sh
  where
    f (_, (x,_,_,_,_,_)) = x
    g (_, (x,b,_,a,_,l)) = (c,x,h)
      where 
        h = inv b <> a
        c = cameraFromAffineHomogZ0 1.7 (h <> diagl[-1,1,1])
    s (_, (_,_,_,_,_,l)) = not (l `elem` ["I","O","0","9","6"])
        
    sh (im,oks) = do
        clear [DepthBuffer]
        drawImage (im::ImageGray)
        clear [DepthBuffer]
        pointCoordinates (mpSize 10)
        depthFunc $= Just Less
        
        mapM_ (h.g) (filter s oks::[OKS])

--- effects

vertical (cam,x,h) = do
    setColor' yellow
    lineWidth $= 3
    let ih = inv (h <> diagl[-1,1,1])
        up = (4 >< 3) [1,0,0
                      ,0,0,0
                      ,0,1,0
                      ,0,0,1]
        ym = minimum $ map py $ polyPts $ transPol ih x
        d1 = desp (0,-ym)
        d2 = (4><4) [1,0,0,0
                    ,0,1,0,ym
                    ,0,0,1,0
                    ,0,0,0,1]
        t = cam <> d2 <> up <> d1 <> ih
    shcont $ transPol t x

axes (cam,x,h) = do
    setColor' yellow
    lineWidth $= 1
    renderPrimitive LineLoop $ mapM_ vertex $ ht cam [[0,0,0],[0,0,5::Double]]
    lineWidth $= 3
    renderPrimitive LineLoop $ mapM_ vertex $ ht cam [[0,0,0],
                                               [0,0,2::Double],
                                               [0,0,0],[2,0,0],[0,0,0],[0,2,0]]




elevated (cam,x,h) = do
    setColor' yellow
    lineWidth $= 1
    let ih = inv (h <> diagl[-1,1,1])
        el = (4 >< 3) [1,0,0
                      ,0,1,0
                      ,0,0,0
                      ,0,0,1]
        ym = minimum $ map py $ polyPts $ transPol ih x
        d3 = (4><4) [1,0,0,0
                    ,0,1,0,0
                    ,0,0,1,2
                    ,0,0,0,1]
        t = cam <> d3 <> el <> ih
        y = transPol t x
    shcont y
    setColor' Col.gray
    shcont x
    renderPrimitive Lines $ mapM_ vertex $ concat $ transpose [polyPts x, polyPts y]


----------------------------------------------------------------------

parseMon = monitor "Parse" (mpSize 10) sh
  where
    f (_, (x,_,_,_,_,_)) = x
    sh (im,oks) = when (length oks > 2) $ do
        drawImage (im::ImageGray)
        pointCoordinates (mpSize 10)
        shcont (chPol $ Closed $ concatMap (polyPts.f) oks)

----------------------------------------------------------------------

classifyMon' cam = monitor "Detected" (mpSize 20) sh cam
  where
    sh (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (f.g) oks
    g (d, (x,b,u,a,v,l)) = (d,((x, inv b <> a),l))
    f (d,((c,m),l)) = do
        setColor' red
        lineWidth $= 3
        shcont c
        setColor' black
        text2D (d2f ox) (d2f up) (printf "%s %.0f" l (d*100))
        setColor' yellow
        --shcont $ (transPol m) (Closed [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1])
        --let ellip = transPol m unitCircle
        --shcont ellip
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)
        up = 2/400 + maximum (map py (polyPts c))


unitCircle = Closed [Point (cos (t*degree)) (sin (t*degree)) | t <- [0,10 .. 350]]


----------------------------------------------------------------------
        
chPol (Closed ps) = Closed (convexHull ps)

dist a b = norm (a-b)

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
        lineWidth $= 3
        mapM_ shcont wcs1
        setColor' blue
        lineWidth $= 3
--        mapM_ shcont wcs2

    f c@(Closed ps) = map (transPol t) . icaConts . whitenContour $ c
      where
        t = desp (ox,oy) <> diagl [0.025,0.025,1]
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

----------------------------------------------------------------------

-- DEMO > analyze pentos
-- show discrimination matrix for different distances in a given problem

analyzeAlign shapes = r
  where
    xs = map (map snd . toCanonicAll . fst) shapes
    r = mat [ [ distMult a b | b <-xs ] | a <- {-[0]:-}xs ]
    distMult us vs = minimum $ dist <$> us <*> vs


analyzeInvar f shapes = r
  where
    xs = map (f.fst) shapes
    r = mat [ [ norm (a-b) | b <-xs ] | a <- {-0:-}xs ]


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
    --shapes <- fmap (filter ((not.(`elem`["9","Y'","J","S","B","Q","N'"])).snd)) gen
    shapes <- fmap (filter ((not.(`elem`["9"])).snd)) gen
    let a = analyzeInvar (simpleInvar maxFreq . fourierPL . whitenContour) shapes
        b = analyzeAlign shapes
        c = analyzeInvar (oldInvar maxFreq . fourierPL . whitenContour) shapes
    --dspnor c
--    dspnor a
    dspnor b
    dspnor $ (b-a)/b

dspnor m = do
    putStr . dispf 0 . (*100) $ m             

dspnor' m = do
    putStr $ latexFormat "bmatrix" (dispf 0 (takeColumns 13 $ 100*m)) 


----------------------------------------------------------------------

