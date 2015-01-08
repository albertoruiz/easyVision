-- cabal install fgl

{-# LANGUAGE RecordWildCards #-}

import Vision.GUI
import Image.Processing
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent(threadDelay)
import Util.Misc(replaceAt)
import Contours(asSegments, isLeft)
import Util.Debug(debug)
import Data.Maybe(catMaybes)
import Contours(momentsContour, transPol, minkowskiConvex)
import Util.Homogeneous(desp)
import Data.List(tails, group, foldl1')
import Util.Geometry
import Data.Graph.Inductive(mkGraph,Gr)
import Data.Graph.Inductive.Query.SP(sp)
import Numeric.LinearAlgebra(toList,linspace,uniformSample)
import Util.Polygon


type L = [Double]

data State = State
    { time    :: Double
    , actions :: L
    , vars    :: L
    , target  :: Point
    , plan    :: [Point]
    , info    :: Bool
    , nodes   :: [Point]
    , arcs    :: [Segment]
    , sensors :: [Segment]
    }
 
main = runT_ clock $ arrL diff >>> lander >>> mapper -- >>> freqMonitor

clock :: IO (IO (Maybe NominalDiffTime))
clock = return (threadDelay 10000 >> Just .  utcTimeToPOSIXSeconds <$> getCurrentTime)

diff :: [NominalDiffTime] -> [Double]
diff (x:xs) = map (realToFrac . subtract x) xs

simulator state0 kbd integrate safe auto display
    = transUI $ interface (Size 500 500) "dynamics" state0 firsttime updts acts result disp
  where
    firsttime _ _ = return ()
    updts = kbd
    acts  = []
    result _roi d t1 = (r, r)
      where
        r = integrate t1 . safe . auto $ d
    disp _roi _pts _input v = display v

kbdAct k j v = [ (      key k, \_roi _pt -> modifAction j v)
               , (kUp $ key k, \_roi _pt -> modifAction j 0) ]

modifAction j v st = st { actions = replaceAt [j] [v] (actions st) } 


thres t x = if x < t then 0 else x

rel a b = abs (a-b)/ a
 


pcircle n r = Polygon [Point (r*cos t) (r*sin t) | t <- as]
  where
    as = init $ toList $ linspace (n+1) (0, 2*pi)

dilatePolygon δ = minkowskiConvex (pcircle 7 δ)

--outer = dilatePolygon 0.11
--inner = dilatePolygon 0.10

outer = minkowskiConvex (triang 0.01 0 0)
inner = minkowskiConvex (triang 0    0 0)

--------------------------------------------------------------------------------

mapper = arrL (scanl f []) >>> sMonitor "SLAM" shmap
  where
    f prev  st =  take 200 $ map g (filter ((<0.195) . segmentLength) (sensors st)) ++ prev
    g (Segment p q) = q .-. p
    shmap _ pts = [pointSz 3 pts]

--------------------------------------------------------------------------------

lander :: ITrans Double State
lander = simulator state0Lander kbdLander integrateLander id autoLander displayLander

state0Lander = State
    { time    = 0
    , actions = [1,0,0,0]
    , vars    = [0,0.9]++[0,0.5]
    , target  = Point 0.8 0.2
    , plan    = []
    , info    = False
    , nodes   = []
    , arcs    = []
    , sensors = []
    }

kbdLander = concat
    [ auto
    , kbdAct (SpecialKey KeyLeft)  1 1
    , kbdAct (SpecialKey KeyRight) 2 1
    , kbdAct (SpecialKey KeyUp)    3 1
    , tgt
    ]
  where
    auto = [ (key (Char 'p'), \_roi _pt st -> let (p,(n,a)) = mkPlan (loc st) (target st) regsObstacles in st { actions = f (actions st), plan = p, nodes = n, arcs = a}) ]
      where
        f (0:as) = (1:as)
        f _ = [0,0,0,0]
    tgt = [ (key (MouseButton LeftButton), \_roi pt st -> let (p,(n,a)) = mkPlan (loc st) pt regsObstacles in st { target = pt, plan = p, nodes = n, arcs = a})
          , (key (Char '1'), \_roi _p st -> st {info = not (info st)}) ]

loc st = Point x y
  where
    [_,x,_,y] = vars st


integrateLander t1 st = st { time = t1, vars = [vx',x',vy',y'], sensors = readSensors x y }
  where
    t0 = time st
    [p,a,b,c] = actions st
    [vx,x,vy,y] = vars st
    dt = (t1-t0)
    vy' = vy + (c-0.4) * dt
    y'  = y + vy'* dt 
    vx' = vx + (a-b) * dt
    x'  = x + vx'* dt

lunarLander col x y a b c = Draw
    [ color col $ fillConvexPolygon (triang 0 x y)
    , lineWd 2 $ color red $ Segment (Point x y1) (Point x (y1-0.1*c))
    , lineWd 2 $ color red $ Segment (Point x y1) (Point (x-0.1*a) y1)
    , lineWd 2 $ color red $ Segment (Point x y1) (Point (x+0.1*b) y1)
    ]
  where    
    y1 = y-0.01

triang δ x y = Polygon [ Point (x-0.05-δ) (y-δ), Point (x+0.05+δ) (y-δ), Point x (y+0.1+δ) ]

displayLander State{..} = Draw
        [ shInfo $ color darkgray $ map fillConvexPolygon regInt
        , color blue $ map fillPolygon obstacles
        , pointSz 7 $ color green $ Point tx ty
        , shInfo $ color orange arcs
        , shInfo $ color orange $ pointSz 5 $ drawPointsLabeled nodes
        , lineWd 3  $ color green tplan
        , text (Point 0.9 0.8) $ printf "t=%7.2fs,   %.0f %.0f %.0f" t a b c
        , color yellow sensors
        , color yellow $ Open $ freepoints
        , lunarLander col x y a b c
        , pointSz 5 $ color green $ centerContour (Closed freepoints)
        ]
  where
    t           = time
    [p,a,b,c]   = actions
    Point tx ty = target
    [vx,x,vy,y] = vars
    col | any (insidePolygon (Point x y)) regInt = orange
        | otherwise = white
    
    tplan = Open (plan++[target])
    shInfo x = if info then x else Draw ()
    
    freepoints = map (\(Segment _ q) -> q) sensors


centerContour (Closed ps) = Point x y
  where
    (x,y,_,_,_) = momentsContour ps

safeLander st = st { plan = plan'}
  where
    minRead = minimumBy (compare `on` segmentLength) (sensors st)
    plan' | null (sensors st) = plan st
          | segmentLength minRead >= 0.15 = plan st
          | null (plan st) = [opposite minRead]
          | otherwise = -- midPoint (opposite minRead) (head (plan st)) : tail (plan st)
                        opposite minRead : tail  (plan st) 

r .*. Point x y = Point (r*x) (r*y)
Point x1 y1 .+. Point x2 y2 = Point (x1+x2) (y1+y2)
Point x1 y1 .-. Point x2 y2 = Point (x1-x2) (y1-y2)
opposite s@(Segment p q) = p .+. ((0.2 * recip (segmentLength s)) .*. d)
  where
    d = p .-. q

midPoint = interPoint 0.5

mkPlan loc target (obstIn,obstOut) = ((map (snd . (nodes!!)) sol), (map snd nodes, map f arcs))
  where
    nodes = zip [0..] $ filter ok $ loc: target: (uniquePoints $ concatMap polygonNodes obstOut)
    ok pt = all (not . insidePolygon pt) obstIn
    arcs = [ (i,j,l) | (i,p) <- nodes, (j,q) <- nodes, let l = distPoints p q, free p q ]
    free p q = freePath (Open [p,q]) (map (Closed . polygonNodes) obstIn)
    graph = mkGraph nodes arcs :: Gr Point Double
    sol = sp 0 1 graph
    f (i,j,l) = Segment (snd $ nodes!!i) (snd $ nodes!!j)
    
uniquePoints =  map head . group . sortBy (compare `on` cp) 
  where
    cp (Point x y) = (x,y)       
   

autoLander st@State{actions = [0,a,b,c]} = st
autoLander st@State{..} = st { actions = [1,a',b',c'], plan = plan' }
  where
    t = time
    [vx,x,vy,y] = vars
    Point tx ty | null plan = target
                | otherwise = head plan
  
    a' = if fx > 0 then 1 else 0
    b' = if fx < 0 then 1 else 0
    c' | t > 2 && vy < vyt = 1
       | t > 2 && vy > vyt = 0
       | otherwise = 0
    dy = y - ty
    vyt = - signum dy * max 0.1 (abs dy)
    dx = x - tx
    vxt = - signum dx * max 0.1 (abs dx)
    fx | t > 2 && abs dx > 0.01 && vx > vxt = -1
       | t > 2 && abs dx > 0.01 && vx < vxt = 1
       | otherwise = 0

    plan' | abs dx < 0.01 && abs dy < 0.01 && not (null plan) = tail plan
          | otherwise = plan


regInt = map inner obstacles
regExt = map outer obstacles
regsObstacles = (regInt, regExt)

hdesp x y = unsafeFromMatrix (desp (x,y)) :: Homography

obstacles =  concatMap convexComponents
            [ hdesp 0.5 0.5    <| trian
            , hdesp 0.7 (-0.2) <| trian 
            , Polygon lab
            ]
  where
    trian = Polygon $ reverse [Point (-0.5) (-0.5), Point (-0.9) (-0.5), Point (-0.9) (-0.9)]

lab = [Point (-0.375) 0.17,Point (-0.61) 0.155,Point (-0.6) 0.87,Point 0.725 0.83,Point 0.66 (-0.51),Point (-0.535) (-0.57),Point (-0.53) (-0.255),Point (-7e-2) (-0.26),Point (-5.5e-2) 0.415,Point 0.15 0.41,Point 0.195 (-0.275),Point 0.47 (-0.29),Point 0.44 0.635,Point (-0.275) 0.685]
--------------------------------------------------------------------------------

readSensors x y = map cut raw
  where
    d = 0.2
    raw = [ Segment (Point x y) (Point (x+d*cos t) (y+d*sin t)) | t <- angles ]
    angles = -- toList $ linspace 14 (5*pi/6,13*pi/6)
             toList $ linspace 18 (0,2*pi)
    cut s@(Segment p q) = minimumBy (compare `on` segmentLength) . map (Segment p) . (q:) . catMaybes . map (segmentIntersection s) $ ssObstacles

ssObstacles = concatMap polygonSides obstacles

--------------------------------------------------------------------------------

freePath :: Polyline -> [Polyline] -> Bool
freePath path obss = all (freeObj path) obss
  where
    freeObj p obj = null $ catMaybes $ [ segmentIntersection a b | a <- as, b <- bs]
      where
        as = asSegments p
        bs = asSegments obj


