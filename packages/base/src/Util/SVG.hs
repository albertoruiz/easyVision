{-# LANGUAGE RecordWildCards #-}

module Util.SVG(
    SVG, mkSVG,
    TransRect(..), mkTransRect,
    Color, Width, Style, Label,
    gstyle, style,
    mpath, pathSimple, PlotShape, pathWith,
    clip,
    circle, rectangle, rectCenter, tMove, tRotate,text,
    -- grid,
    range, 
    graph,
    mkPlot, plotColor, PlotStyle, plotSymbol, circles, squares, legendStyles,  
    vbar, hbar, vbars, hbars, boxdis, legendSquares,
    animate,
    Point(..),
    simplePlot, PlotOptions(..), PlotItem(..), (~>)
)where

import Numeric.LinearAlgebra(toLists,toList,linspace,Vector,minElement,maxElement)
import Util.Geometry(Point(..),datMat)
import Text.Printf
import Util.Text(replace, Rule((:>)))
import Util.Misc(quartiles)
import Control.Arrow((&&&))
import Data.List(zipWith4)

data TransRect = TR{
    tX :: Double -> Double,
    tY :: Double -> Double,
    tP :: Point -> Point,
    tPoint :: Double -> Double -> Point,
    tdX :: Double -> Double,
    tdY :: Double -> Double,
    itX :: Double -> Double,
    itY :: Double -> Double,
    itdX :: Double -> Double,
    itdY :: Double -> Double
}


range :: Int -> (Double,Double) -> [Double]
range n = toList . linspace n

mkTransRect :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> TransRect
mkTransRect x1' x2' y1' y2' x1 x2 y1 y2 = TR{..}
  where
    tX = t x1 x2 x1' x2'
    tY = t y1 y2 y1' y2'
    tP (Point x y) = (Point (tX x) (tY y))
    tPoint x y = tP (Point x y)
    tdX dx = tX dx - tX 0
    tdY dy = tY dy - tY 0
    itX = t x1' x2' x1 x2
    itY = t y1' y2' y1 y2
    itdX dx = itX dx - itX 0
    itdY dy = itY dy - itY 0
    t a1 a2 b1 b2 = f
      where
        s = (b2-b1)/(a2-a1)
        f x = b1+s*(x-a1)



mkSVG
    :: Int      -- width
    -> Int      -- height
    -> [SVG]    -- objects to include
    -> String   -- result
mkSVG w h = header w h . unlines
    
header :: Int -> Int -> String -> String
header w h x = unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" 
    , r "<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='W' height='H' >"
    , x
    ,"</svg>"
    ]
 where
   r = replace ["W":> show w, "H":> show h]

type Color = String
type Width = Double
type SVG = String
type Style = String
type Label = String


(%>) :: String -> Double -> Rule
s %> r = s :> printf "%.3f" r


gstyle :: Style -> [SVG] -> SVG
gstyle s x = unlines
    [ replace r "<g style='S'>"
    , unlines x
    , "</g>"
    ]
  where
    r = [ "S"   :> s
        ]


style :: Color -> Width -> Color -> [SVG] -> SVG
style col width fill x = unlines
    [ replace r "<g style='fill:FILL; stroke:COL; stroke-width:WIDTH'>"
    , unlines x
    , "</g>"
    ]
  where
    r = [ "COL"   :> col
        , "WIDTH" :> show width
        , "FILL"  :> fill
        ]

mpath :: [Point] -> SVG
mpath ps =
    replace
      ["PTS"   :> (concatMap p . toLists . datMat) ps]
      "<path  d = 'M PTS' />"
  where
    p [x,y] = printf "%.3f %.3f " x y
    p _ = error "mpath"




pathSimple :: Color -> Width -> [Point] -> SVG
pathSimple col width = style col width "none" . return .  mpath

circle :: Double -> Point -> SVG
circle r (Point x y) = replace
    ["CX" %> x, "CY" %> y, "R" %> r ]
    "<circle cx='CX' cy='CY' r='R' />"

rectangle :: Point -> Point -> SVG
rectangle (Point x1 y1) (Point x2 y2) = replace
    ["X" %> x, "Y" %> y, "W" %> w, "H" %> h ]
    "<rect x='X' y='Y' width='W' height='H' />"
  where
    x = min x1 x2
    y = min y1 y2
    w = abs (x2-x1)
    h = abs (y2-y1)

rectCenter :: Double -> Double -> Point -> SVG
rectCenter rx ry (Point xc yc) = replace
    ["CX" %> x, "CY" %> y, "W" %> w, "H" %> h ]
    "<rect x='CX' y='CY' width='W' height='H' />"
  where
    x = xc - rx
    y = yc - ry
    w = 2*rx
    h = 2*ry

text :: Double -> Point -> String -> SVG
text sz (Point x y) t = replace
    ["X" %> x, "Y" %> y, "T" :> t, "S" :> show sz ]
    "<text x='X' y='Y' style='font-size:Spx'> T </text>"

clip :: SVG -> [SVG] -> SVG
clip g s = unlines
    [ "<defs> <clipPath id='clip"++name++"'>"
    , g
    , "</clipPath> </defs>"
    , "<g clip-path='url(#clip"++name++")'>"
    , unlines s
    , "</g>" ]
  where
    name = filter (`elem` ['0'..'9']) g

-----------------------------------------------------

vbar :: Point -> Double -> Double -> SVG
vbar (Point xc yc) l w = replace
    ["CX" %> x, "CY" %> y, "W" %> w, "H" %> h ]
    "<rect x='CX' y='CY' width='W' height='H' />"
  where
    x = xc - w/2
    y = if l>=0 then yc-l else yc
    h = abs l

hbar :: Point -> Double -> Double -> SVG
hbar (Point xc yc) l w = replace
    ["CX" %> x, "CY" %> y, "W" %> w', "H" %> h ]
    "<rect x='CX' y='CY' width='W' height='H' />"
  where
    y = yc - w/2
    x = if l>=0 then xc else xc + l
    w' = abs l
    h = w


--------------------------------------------------------

tRotate :: Point -> Double -> [SVG] -> SVG
tRotate (Point x y) a = tMove (x,y) . return . tRot0 (-a) . return . tMove (-x,-y)


tRot0 :: Double -> [SVG] -> SVG
tRot0 a s = unlines
    [ r "<g transform='rotate(A)'>"
    , unlines s
    , "</g>" ]
 where
   r = replace ["A":>show a]


tMove :: (Double,Double) -> [SVG] -> SVG
tMove (x,y) s = unlines
    [ r "<g transform='matrix(1,0,0,1,X,Y)'>"
    , unlines s
    , "</g>" ]
 where
   r = replace ["X":>show x, "Y":>show y]

circles :: PlotShape
circles r = unlines . map (circle r)

squares :: PlotShape
squares r = unlines . map (rectCenter r r)

type PlotShape = Double -> [Point] -> SVG

pathWith :: PlotShape -> Color -> Width -> Color -> Double -> [Point] -> SVG
pathWith f coll w colc r ps = unlines
    [ pathSimple coll w ps 
    , style "none" 1 colc [f r ps]
    ]

vbars :: Point -> Double -> Double -> [Double] -> SVG
vbars (Point x y) w s ls = unlines $ zipWith f [0..] ls
  where
    f k l = vbar (Point (x+s*k*w) y) l w


hbars :: Point -> Double -> Double -> [Double] -> SVG
hbars (Point x y) w s ls = unlines $ zipWith f [0..] ls
  where
    f k l = hbar (Point x (y+s*k*w)) l w


boxdis :: TransRect -> Double -> Double -> [Double] -> SVG
boxdis TR{..} w x ds = unlines
    [ mpath [tPoint x l, tPoint x q1]
    , mpath [tPoint x q2, tPoint x u]
    , mpath [tPoint (x-w4) l, tPoint (x+w4) l]
    , mpath [tPoint (x-w4) u, tPoint (x+w4) u]
    , vbar (tPoint x q1) h w
    , mpath [tPoint (x-w2) m, tPoint (x+w2) m]
    ]  
  where
    (l,q1,m,q2,u) = quartiles ds
    w2 = itdX (w/2)
    w4 = itdX (w/4)
    h = abs (tdY (q2-q1))

--------------------------------------------------------------------------------

grid
  :: TransRect
     -> Style
     -> Double
     -> Double
     -> [a]
     -> (a -> Double)
     -> (a -> String)
     -> String
     -> Double
     -> Double
     -> [b]
     -> (b -> Double)
     -> (b -> [Char])
     -> String
     -> String
grid t sty x1 x2 rxs fpx fsx xlabel y1 y2 rys fpy fsy ylabel = unlines
    [ xaxis t xlabel y1 x1 x2 rxs fpx fsx
    , yaxis t ylabel x1 y1 y2 rys fpy fsy
    , gstyle sty [ xtics t y1 y2 xs
                 , ytics t x1 x2 ys
                 ]
    , gstyle sty2 [ xtics' t y1 y2 xs , ytics' t x1 x2 ys ]
    ]
  where
    xs = map fpx rxs
    ys = map fpy rys
    sty2 = "stroke:black; stroke-width:1"


xaxis
  :: TransRect
     -> String
     -> Double
     -> Double
     -> Double
     -> [b]
     -> (b -> Double)
     -> (b -> String)
     -> SVG
xaxis TR{..} name y x1 x2 xs fp fs = gstyle "text-anchor:middle"
    [ unlines $ map (g . (fp &&& fs)) xs
    , text 12 (Point ((tX ((x1+x2)/2))) (tY y +2*st)) name
    ]
  where
    st = 16
    g (x,s) = text 12 (Point (tX x) (tY y +st)) s


yaxis
  :: TransRect
     -> String
     -> Double
     -> Double
     -> Double
     -> [b]
     -> (b -> Double)
     -> (b -> [Char])
     -> String
yaxis TR{..} name x y1 y2 ys fp fs = unlines
    [ gstyle "text-anchor:end" $ map (g . (fp &&& fs)) ys
    , gstyle "text-anchor:middle" [rot $ text 12 (Point clx cly) name]
    ]
  where
    wy = maximum $ map (length.fs) ys
    st = 8
    g (y,s) = text 12 (Point (tX x -st) (tY y+4-1)) s
    clx = tX x -(fromIntegral wy+2)*st
    cly = tY ((y1+y2)/2)
    rot = tRotate (Point clx cly) 90 . return

xtics :: TransRect -> Double -> Double -> [Double] -> String
xtics TR{..} y1 y2 = unlines . map f
  where
    f x = mpath [tPoint x y1, tPoint x y2]


xtics' :: TransRect -> Double -> Double -> [Double] -> String
xtics' TR{..} y1 y2 = unlines . map f
  where
    f x = mpath [tPoint x y1, tPoint x (y1+dy )]
    dy = (y2-y1)/50


ytics :: TransRect -> Double -> Double -> [Double] -> String
ytics TR{..} x1 x2 = unlines . map f
  where
    f y = mpath [tPoint x1 y, tPoint x2 y]

ytics' :: TransRect -> Double -> Double -> [Double] -> String
ytics' TR{..} x1 x2 = unlines . map f
  where
    f y = mpath [tPoint x1 y, tPoint (x1+dx) y]
    dx = (x2-x1)/50


--------------------------------------------------------------------------------

graph :: Point
      -> Point
      -> Color
      -> Style
      -> Double
      -> Double
      -> Label
      -> [a]
      -> (a -> Double)
      -> (a -> String)
      -> Double
      -> Double
      -> Label
      -> [b]
      -> (b -> Double)
      -> (b -> String)
      -> ([SVG] -> SVG, TransRect)
graph (Point w1 h1) (Point w2 h2) color gridstyle
      x1 x2 xlabel xs fpx fsx
      y1 y2 ylabel ys fpy fsy 
      = (diagram,t)
  where
    t = mkTransRect w1 w2 h2 h1 x1 x2 y1 y2
    region = rectangle (Point w1 h1) (Point w2 h2)
    diagram ds = unlines
        [ style "none" 1 color [region] 
        , grid t gridstyle
               x1 x2 xs fpx fsx xlabel
               y1 y2 ys fpy fsy ylabel
        , style "black" 1.5 "none" [region]
        , clip region ds
        ] 

--------------------------------------------------------------------------------

plotV
  :: TransRect
     -> [(Color, Width, PlotShape, Color, Double)]
     -> [(Vector Double, Vector Double)]
     -> String
plotV _ [] _ = []
plotV TR{..} ss xys = unlines $ zipWith (plotV1 tPoint) ss xys
  where
     plotV1 tp (coll, w, f, colc, r) (x,y)
        = pathWith f coll w colc r (zipWith tp (toList x) (toList y))


autoRange :: Double -> Double -> Double -> [Double]
autoRange delta a b = filter (>=a) $ filter (<=b) [l*delta,(l+1)*delta .. u*delta]
  where
    u = fromIntegral $ (ceiling $ b/delta ::Int)
    l = fromIntegral $ (floor $ a/delta ::Int)


autoGraph
  :: Point
     -> Point
     -> Color
     -> Style
     -> Label
     -> Double
     -> Double
     -> Double
     -> String
     -> Label
     -> Double
     -> Double
     -> Double
     -> String
     -> ([SVG] -> SVG, TransRect)
autoGraph p1 p2 col gridcol xlab x1 x2 dx sfx ylab y1 y2 dy sfy
    = graph p1 p2 col gridcol
            x1 x2 xlab (autoRange dx x1 x2) id (printf sfx)
            y1 y2 ylab (autoRange dy y1 y2) id (printf sfy)

mkPlot :: Int
       -> Int
       -> String
       -> Color
       -> Color
       -> Style
       -> Label
       -> Double
       -> Double
       -> Double
       -> String
       -> Label
       -> Double
       -> Double
       -> Double
       -> String
       -> [PlotStyle]
       -> [(Vector Double, Vector Double)]
       -> Point
       -> Style
       -> Double
       -> [Label]
       -> SVG 
mkPlot w h title colb col colg xlab x1 x2 dx sfx ylab y1 y2 dy sfy stys ps loc lsty sz names = mkSVG w h
    [ back
    , tit
    , mkg [ plotV t stys ps ]
    , if null names then "" else legendStyles (tP loc) lsty sz stys names
    ]
  where
    (mkg,t@TR{..}) = autoGraph (Point 70 50) (Point (fromIntegral w-40) (fromIntegral h-50)) col colg
         xlab x1 x2 dx sfx
         ylab y1 y2 dy sfy
    back = if colb == "none" || colb == ""
            then ""
            else style "none" 1 colb [rectangle (Point 0 0) (Point (fromIntegral w) (fromIntegral h))]
    tit = if title==""
            then ""
            else gstyle "text-anchor:middle" [text 14 (Point (fromIntegral w / 2) 30) title]

legendStyles :: Point -> Style -> Double -> [PlotStyle] -> [Label] -> SVG
legendStyles loc bsty sz stys' names' =
    unlines [ lback 
            , clipit  $ zipWith f stys (take (length names) samples)
            , unlines $ zipWith g names ys
            ]
  where
    (stys,names) = unzip . filter (not.null.snd) $ zip stys' names'
    Point x y = loc
    delta = sz*1.7
    ys = [0,delta..]
    samples = [[Point x (y+dy), Point (x+delta) (y+dy), Point (x+2*delta) (y+dy)]|dy<-ys]
    clipit = clip (rectangle (Point (x+5) (y-delta)) (Point (x+2*delta-5) (y+hmax*delta)))
    f (coll, w, h, colc, r) = pathWith h coll w colc r
    g name dy = text sz (Point (x+5+2*delta) (y+dy+3)) name
    lback = gstyle bsty [rectangle (Point (x-5) (y-delta)) (Point (x+wmax*sz*0.6+delta*2.5) (y+hmax*delta))]
    wmax = fromIntegral (maximum (map length names))
    hmax = fromIntegral (length names)


type PlotStyle = (Color,Width,PlotShape,Color,Double)

plotColor :: Color -> Width -> PlotStyle
plotColor col w = (col,w,\_ _ -> "","",0)

plotSymbol :: Color -> Width -> PlotShape -> Color -> Double -> PlotStyle
plotSymbol coll w symb cols r = (coll,w,symb,cols,r)

--------------------------------------------------------------------------------

legendSquares :: Point -> Style -> Double -> [Color] -> [Label] -> SVG
legendSquares p@(Point x0 y0) bsty sz cols msgs = unlines
    [ gstyle bsty [rectangle (Point (x0-0.7 *w*sz) (y0-sz)) (Point (x0+2*sz) (y0+1.5*h*sz))]
    , colbars p [1,1..]
    ]
  where
    h = fromIntegral (length msgs) + 0.5
    w = fromIntegral (maximum (map length msgs))

    colbarh (Point x y) l col msg = gstyle "text-anchor:end"
      [ style "black" 1 col [rectangle (Point x y) (Point (x+sz*l) (y+sz))]
      , text (sz) (Point (x-sz/2) (y+0.9*sz)) msg
      ]

    colbars (Point x y) ls = unlines $ zipWith4 colbarh [Point x y' | y' <- [y,y+1.5*sz .. ]] ls cols msgs



--------------------------------------------------------------------------------

animate :: String -> [SVG] -> [SVG]
animate pre xs = zipWith f [0..] xs
  where
    f k g = gstyle ("visibility:hidden' id='" ++ pre ++ show (k :: Int)) [g] 

--------------------------------------------------------------------------------

data PlotOptions
    = Labels String String
    | LegendPos Double Double
    | GridSty String
    | LegendSty String
    | Title String
    | PlotSize Int Int
    | VisibleSize Int Int
    | BKColor String
    | DeltaX Double
    | DeltaY Double
    | DecX Int
    | DecY Int
    | MarginX Double
    | MarginY Double
    | MinX Double
    | MaxX Double
    | MinY Double
    | MaxY Double



defaultPlot :: [PlotOptions]
defaultPlot =
    [ LegendPos 0.75 0.25
    , Labels "" ""
    , Title ""
    , PlotSize 600 400
    , BKColor "white"
    , GridSty "stroke:black;stroke-width:0.1;stroke-dasharray:2"
    , LegendSty "fill:#fcfcff;stroke:gray"
    , MarginX 0
    , MarginY 0.05
    ]

data PlotItem = PlotItem
    { plotxy  :: (Vector Double, Vector Double)
    , plotsty :: PlotStyle
    , plotlab :: String
    }

(~>) :: (Vector Double, Vector Double) -> (PlotStyle, String) -> PlotItem
x ~> (p,l) = PlotItem x p l

simplePlot :: [PlotOptions] -> [PlotItem] -> SVG
simplePlot opts things = mkPlot w h title "none" bkcol gridsty 
    labelx x1m x2m deltax ("%."++show decx++"f")
    labely y1m y2m deltay ("%."++show decy++"f")
    stys
    xys
    (Point lx ly) legsty 12 labs
  where
    xys  = map plotxy  things
    stys = map plotsty things
    labs = map plotlab things
    x1   = minimum (map (minElement.fst) xys)
    x2   = maximum (map (maxElement.fst) xys)
    y1   = minimum (map (minElement.snd) xys)
    y2   = maximum (map (maxElement.snd) xys)
    x1m  = last [ z | MinX z <- MinX (x1 - (x2-x1)*marginx) : ops ]
    x2m  = last [ z | MaxX z <- MaxX (x2 + (x2-x1)*marginx) : ops ]
    y1m  = last [ z | MinY z <- MinY (y1 - (y2-y1)*marginy) : ops ]
    y2m  = last [ z | MaxY z <- MaxY (y2 + (y2-y1)*marginy) : ops ]
    lx   = x1m + (x2m-x1m) * labelxpos
    ly   = y1m + (y2m-y1m) * labelypos
    ops = defaultPlot ++ opts
    (w, h) = last $ [(x,y) | PlotSize x y <- ops ] ++ [(x+110,y+100) | VisibleSize x y <- ops ]
    (labelxpos, labelypos) = last [(x,y) | LegendPos x y <- ops ]
    (labelx, labely) = last [(x,y) | Labels x y <- ops ]
    title = last [x | Title x <- ops ]
    bkcol = last [x | BKColor x <- ops ]
    gridsty  = last [x | GridSty x <- ops ]
    legsty = last [x | LegendSty x <- ops ]
    marginx = last [x | MarginX x <- ops ]
    marginy = last [x | MarginY x <- ops ]
    ordx = orderOf (x2-x1)
    ordy = orderOf (y2-y1)
    deltax = improve (x2-x1) $ last [x | DeltaX x <- DeltaX (10**fromIntegral(ordx-1)) : ops ]
    deltay = improve (y2-y1) $ last [x | DeltaY x <- DeltaY (10**fromIntegral(ordy-1)) : ops ]
    decx = max 0 $ fromIntegral $ last [x | DecX x <- DecX (max 0 (1-ordx)) : ops ] :: Int
    decy = max 0 $ fromIntegral $ last [x | DecY x <- DecY (max 0 (1-ordy)) : ops ] :: Int
    orderOf x = round $ logBase 10 x
    improve rang delta
        | n > 10 = 2*delta
        | otherwise = delta
      where
        n = rang / delta

