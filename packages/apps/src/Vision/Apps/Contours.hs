{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Vision.Apps.Contours (
    contours, contours',
    injectPrototypes,
    showCanonical,
    showAlignment,
    module Contours
) where

import Vision.GUI
import ImagProc
import Contours
import Control.Arrow((***),(&&&))
import Control.Applicative
import Numeric.LinearAlgebra((<>),fromList,inv)
import Text.Printf(printf)
import Util.Misc(diagl,mean,vec,debug,degree)
import Util.Rotation(rot3)
import Util.Options
import Vision(desp,inHomog,hv2pt)
import Classifier(Sample)
import Control.Monad(when)
import Data.List(minimumBy,sortBy)
import ImagProc.C.NP(npcontours, winNPParam)
import qualified Features.Polyline as Feat
import Data.Function(on)



autoParam "ContourParam" "contour-" [
    ("athres",  "Int",    intParam 1 (-20) 10),
    ("thres",  "Int",    intParam 128 1 255),
    ("area",   "Int",    percent 10),
    ("fracpix","Double", realParam (1.5) 0 10),
    ("mode",   "String", stringParam "black" ["white", "black", "both"]),
    ("auto",   "Int",    intParam 1 0 1),
    ("smooth", "Int",    intParam 1 0 10),
    ("asmooth", "Int",    intParam 0 0 10),
    ("thresDelta", "Int",    intParam 16 0 255),
    ("thresRange", "Int",    intParam 0 0 10),
    ("radius", "Int",    intParam 10 0 30)]


autoParam "PolygonParam" "polygon-"
  [( "eps","Double" ,realParam 10 0 50),
   ( "sides","Int" ,intParam 4 3 10),
   ( "tol","Double" ,realParam 10 0 45)]


contours :: ITrans ImageGray (ImageGray, [Polyline])
contours = arr id &&& (npcontours @@@ winNPParam >>> arr (fst.fst))

contours' :: ITrans ImageGray (ImageGray, [Polyline])
contours' = arr id &&& (smartContours id @@@ winContourParam >>> arr contSel) 

catalog :: FilePath -> IO (Sample Polyline)
catalog defaultdb = (read <$> readFile defaultdb) >>= optionFromFile "--catalog"


injectPrototypes :: Renderable t => FilePath -> ITrans (t, [Shape]) ((t, [Shape]), [(Shape, String)])
injectPrototypes defaultdb = transUI $ do
    c <- catalog defaultdb
    let prepro = id
        disp = Draw . transPol (diagl [0.8, 0.8, 1]) . boxShape . shapeContour
    bro <- browseLabeled "Shapes" (map (shape *** id) c) disp

    let ft _ _  = return ()
        result _r _s (x,cs) = (ss,(x,ss))  -- save contours in the state
          where
            ss = map prepro cs
        display _r _s (im,conts) = Draw [ Draw im, color orange
                                        , Draw [map (Draw . shapeContour) conts] ]
        add _r _p [] = return ()
        add _r p cs = updateW bro (id *** ((c,"new"):))  -- add contour to b state
          where
            c = closestTo p cs
        acts = [(key (MouseButton LeftButton), add)]
    r1 <- interface (Size 300 300) "Raw Contours" [] ft [] acts result display

    return $ \c -> do
        a <- r1 c
        b <- getW bro
        return (a,snd b)


closestTo pt = minimumBy (compare `on` (d pt))
      where
        d p c = distPoints p (cen c)
        cen Shape { shapeMoments = (cx,cy,_,_,_) } = Point cx cy

--------------------------------------------------------------------------------

showCanonical :: ITrans (ImageGray, [Shape]) (ImageGray, [Shape])
showCanonical = sMonitor "canonical" disp 
  where
    disp _ (x,ss) = [fun h white 1, fun g yellow 3] 
      where
        fun t col w = Draw [ Draw x
                           , lineWd w, color col, Draw (map (Draw .t) ss) ]

    g Shape {..}  = transPol t (head kShapes)
      where
        t = desp (ox,oy) <> diagl [0.05,0.05,1]
        (ox,oy,_,_,_) = shapeMoments
 
    h Shape {..}  = dirs
      where
        (ox,oy,_,_,_) = shapeMoments
        dirs = Closed $ concatMap ((\x->[Point ox oy, x]) 
                                   . hv2pt.(<> vec [2,0,1])
                                   . inv .  (<> shapeWhitener). snd) kHyps

----------------------------------------------------------------------

showAlignment :: ITrans (ImageGray,[[ShapeMatch]]) (ImageGray,[[ShapeMatch]])
showAlignment = sMonitor "Detected" disp
  where
    disp _ (im,oks) = [ msgM 1 "Invariant"
                      , msgM 0 "Invariant"
                      , msgM 2 "Alignment"
                      ]
      where
        msgM k name = msg name (map (sh k) oks)
        msg name x = Draw [ Draw im
                          , Draw x
                          , color lightgreen
                          , text (Point 0.9 (-0.65)) info
                          , text (Point 0.9 0.65) name ]
        e = (100*) $ mean $ map (invDist.head) $ filter (not.null) oks
        -- mean classification error x100
        np = mean $ map (fromIntegral.length.polyPts.shapeContour.target.head)
           $ filter (not.null) oks
        info = printf "N = %d, np = %.0f, E = %.0f" (length oks) (np::Double) e
        labs = concatMap label

        sh _ [] = Draw ()

        sh 0 (ShapeMatch {..} : ps) = Draw [ color white
                                           , textAtShape target info ]
          where
            info = printf "[%d] %s (%.0f) %s" (length $ polyPts $ shapeContour target)
                                              label (invDist*100) (labs ps)

        sh 1 (ShapeMatch {..} : ps) = Draw [ color (col invDist)
                                           , Draw (bounding (shapeContour target))
                                           , color yellow, textAtShape target zs ]
          where
            zs = if (invDist*100 < 20)
                    then printf "%s" (label++labs ps)
                    else ""                                   
            col d | d < 0.1 = blue
                  | d < 0.2 = red
                  | otherwise = black

        sh 2 (ShapeMatch {..} : _) = Draw [ color yellow, Draw (shapeContour target)
                                          , color red, Draw (transPol wa $ shapeContour proto)
                                          , color white, textAtShape target info ]
            where
              info = printf "%.f" (100*alignDist)

textAtShape s = textF Helvetica10 (centerOf s)
centerOf Shape { shapeMoments = (ox,oy,_,_,_) } = Point ox oy

--------------------------------------------------------------------------------

polygonalize PolygonParam {..} = id *** selectPolygons (eps/1000) sides . map (cleanPol (cos $ tol*degree))

----------------------------------------------------------------------

autoThres r x = sh d
  where
    f = float x
    s = filterBox r r f
    d = f |-| s
    sh = scale32f8u (-1) 1




data ContourInfo = ContourInfo {
    contWhite :: [Polyline],
    contBlack :: [Polyline],
    contBoth  :: [Polyline],
    contSel   :: [Polyline]
    }



defContourParam :: ContourParam
argContourParam :: IO ContourParam
winContourParam :: IO (IO ContourParam)


smartContours :: (x -> ImageGray) -> ContourParam -> x -> ContourInfo
smartContours g ContourParam{..} x = r
    where pre 0 = (smooth `times` median Mask3x3) . g
          pre 1 = (asmooth `times` median Mask3x3) . autoThres radius . g
          z = pre auto x
          pixarea = h*w*area`div`10000 where Size h w = size z
              
          rawg 1 True y = map fst3 $ contours 100 pixarea (128-fromIntegral athres) True y
          rawg 1 False y = map fst3 $ contours 100 pixarea (128+fromIntegral athres) False y

          rawg 0 b y = concatMap (\th -> map fst3 $ contours 100 pixarea (fromIntegral th) b y)
                     [thres-thresDelta*thresRange,
                      thres-thresDelta*(thresRange-1) .. 
                      thres+thresDelta*thresRange]

          cw = post $ rawg auto True z
          cb = post $ rawg auto False z
          cwb = cw ++ cb
          cs = case mode of
                    "white" -> cw
                    "black" -> cb
                    "both"  -> cwb
                    _ -> error "smartContours unknown mode"
          redu | fracpix > 0.01 = douglasPeuckerClosed fracpix
               | otherwise   = id
          clo  = Closed . pixelsToPoints (size z)
          times n f = (!!n) . iterate f
          fst3 (a,_,_) = a
          post = map (clo . redu)
          r = ContourInfo { contWhite = cw
                          , contBlack = cb
                          , contBoth  = cwb
                          , contSel   = cs }
          contours = Feat.contours


