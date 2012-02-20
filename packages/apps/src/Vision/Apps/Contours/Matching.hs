{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Vision.Apps.Contours.Matching (
    winMatching,
    injectPrototypes,
    showCanonical,
    showAlignment
) where

import Vision.GUI
import ImagProc
import Contours
import Control.Arrow((***),(&&&))
import Control.Applicative
import Numeric.LinearAlgebra((<>),fromList,inv,eigenvalues,magnitude,toList,subMatrix,meanCov, fromRows)
import Text.Printf(printf)
import Util.Misc(diagl,mean,vec,debug,degree,median)
import Util.Rotation(rot3)
import Util.Options
import Vision(desp,inHomog,hv2pt)
import Classifier(Sample)
import Control.Monad(when)
import Data.List(minimumBy,sortBy)
import Data.Function(on)

--------------------------------------------------------------------------------

autoParam "MatchingPar" "match"
    [ ("errInvar", "Double", realParam 0.3 0 1)
    , ("errAlign", "Double", realParam 0.3 0 1)
    , ("tolRot",   "Double", realParam 30 0 90)
    , ("vertical", "Int",    intParam  0  0 1)
    , ("minRat",   "Double", realParam  0.3  0 1)
    , ("maxRat",   "Double", realParam  2  1 5)]

winMatching :: ITrans ((t, [Shape]), Sample Shape) (t, [[ShapeMatch]])
winMatching = withParam f
  where
    f mp@MatchingPar{..} =   matchShapes errInvar errAlign
                         >>> (id *** consistent mp mode)
      where
        mode = if vertical == 1 then Just 0 else Nothing

--------------------------------------------------------------------------------

asym = map return "12347¢ABCDEFGJKLMPQRTUVYW" :: [String]
sym  = map return "SHNXZ" :: [String]
ambi = map return "0OI" :: [String]


consistent tr Nothing ms = consistent tr (Just dir) ms
  where
    dir = vmed . (++[0]) . concatMap (map waRot . filter ((`elem` asym).label). take 1) $ ms
    vmed = (\[x,y]->atan2 y x) . toList . fst . meanCov . fromRows . map (\a -> fromList[cos a, sin a])


consistent MatchingPar{..} (Just dir) ms = fixambig2 $ map (map fixambig1 . filter ok) ms
  where
    opos x = if x > 0 then x - pi else x + pi
    angdif x y = min (abs (x-y)) (abs (opos x - opos y))
    ok m = ( angdif (waRot m) dir < tolRot * degree
             || (label m `elem` sym && angdif (opos (waRot m)) dir < tolRot * degree)
             || label m `elem` ambi )
           && waScaleRat m > minRat && waScaleRat m < maxRat
           && (not (label m `elem` ambi) || alignDist m < 0.1)
    fixambig1 s@ShapeMatch {..} | label `elem` sym = s { wa = r <> wa }
                                | otherwise = s
      where
        r = if angdif waRot dir > 90 * degree then rotAround (centerOf target) pi else rot3 0

    fixambig2 ms | null oks = ms
                 | otherwise = map (map f) ms
      where
        oks = concatMap (filter (not . (`elem` ambi) . label). take 1) ms
        closestTo s = minimumBy (compare `on` g s) oks
          where
            g s1 s2 = distPoints (centerOf (target s1)) (centerOf (target s2))
        f s | label s `elem` ambi = s { wa = {-debug "closest" (const $ label t) $  -} d <> wa t
                                      , waRot = waRot t
                                      , waSkew = waSkew t
                                      , waScaleRat = waScaleRat t }
            | otherwise = s
          where
            t = closestTo s
            d = desp (dx,dy)
            dx = x2-x1
            dy = y2-y1
            Point x1 y1 = centerOf (shape $ inducedBounding t)
            Point x2 y2 = centerOf (shape $ inducedBounding s)

inducedBounding ShapeMatch {..} = transPol wa $ bounding (shapeContour proto)

rotAround (Point x y) a = desp (x,y) <> rot3 a <> desp (-x,-y)

--------------------------------------------------------------------------------


catalog :: [FilePath] -> IO (Sample Polyline)
catalog defaultdbs = concat <$> mapM r defaultdbs >>= optionFromFile "--catalog"
  where
    r x = read <$> readFile x


--injectPrototypes :: Renderable [t] => [FilePath] -> ITrans (t, [Shape]) ((t, [Shape]), [(Shape, String)])
injectPrototypes defaultdbs = transUI $ do
    c <- catalog defaultdbs
    let prepro = id
        disp = Draw . transPol (diagl [0.8, 0.8, 1]) . shapeContour
    bro <- browseLabeled "Shapes" (map (shape.boxShape *** id) c) disp

    let ft _ _  = return ()
        result _r _s (x,cs) = (ss,(x,ss))  -- save contours in the state
          where
            ss = map prepro cs
        display _r _s (im,conts) = Draw [ Draw im,
                                          color orange $ map (Draw . shapeContour) conts ]
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
                           , lineWd w . color col $ (map (Draw .t) ss) ]

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
showAlignment = sMonitor "detected" disp
  where
    disp _ (im,oks) = [ msgM 3 "best match"
                      , msgM 5 "inverse bounding box"
                      , msgM 4 "align info "
                      , msgM 1 "invariant"
                      , msgM 0 "invariant"
                      , msgM 2 "alignment"
                      ]
      where
        msgM k name = msg name (map (sh k) oks)
        msg name x = Draw [ Draw im
                          , Draw x
                          , color lightgreen
                              [ text (Point 0.9 (-0.65)) info
                              , text (Point 0.9 0.65) name
                              ]
                          ]
        e = (100*) $ mean $ map (invDist.head) $ filter (not.null) oks
        -- mean classification error x100
        np = mean $ map (fromIntegral.length.polyPts.shapeContour.target.head)
           $ filter (not.null) oks
        info = printf "N = %d, np = %.0f, E = %.0f" (length oks) (np::Double) e
        labs = concatMap label

        sh _ [] = Draw ()

        sh 0 (ShapeMatch {..} : ps) = Draw [ color white
                                           $ textAtShape target info ]
          where
            info = printf "[%d] %s (%.0f) %s" (length $ polyPts $ shapeContour target)
                                              label (invDist*100) (labs ps)

        sh 1 (ShapeMatch {..} : ps) = Draw [ color (col invDist)
                                           $ (bounding (shapeContour target))
                                           , color yellow $ textAtShape target zs ]
          where
            zs = if (invDist*100 < 20)
                    then printf "%s" (label++labs ps)
                    else ""                                   
            col d | d < 0.1 = blue
                  | d < 0.2 = red
                  | otherwise = black

        sh 2 (ShapeMatch {..} : _) = Draw [ color yellow (shapeContour target)
                                          , color red (transPol wa $ shapeContour proto)
                                          , color white $ textAtShape target info ]
            where
              info = printf "%.f" (100*alignDist)

        sh 3 (ShapeMatch {..} : _) = Draw [ color orange bb2
                                          , color yellow $ textAtShape target label ]
          where
            bb2 = transPol wa $ bounding (shapeContour proto)
        sh 5 (ShapeMatch {..} : _) = Draw [ color green bb2
                                          , color lightgreen axes
                                          , color yellow $ textAtShape target label ]
          where
            bb2 = transPol wa . bounding . transPol (inv wa) . shapeContour $ target
            axes = Open $ take 2 (drop 2 $ polyPts bb2)
        sh 4 (ShapeMatch {..} : _) = Draw [ color orange  bb
                                          , color red  axes
                                          , color yellow $ textAtShape target info ]
          where
            info = label ++ printf " %.0f %.0f %.2f" (waRot/degree) (waSkew/degree) (waScaleRat)
            bb = (transPol wa $ bounding (shapeContour proto))
            axes = Open $ take 2 (drop 2 $ polyPts bb)

textAtShape s = textF Helvetica10 (centerOf s)
centerOf Shape { shapeMoments = (ox,oy,_,_,_) } = Point ox oy

--------------------------------------------------------------------------------

