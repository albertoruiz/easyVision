{-# LANGUAGE Arrows, DoRec, RecordWildCards, TemplateHaskell, BangPatterns #-}

import Vision.GUI
import ImagProc
import Vision.Apps.Show(showVectorField)
import Data.List(tails,foldl1')
import Util.Misc(debug)

autoParam "STSmoothP" "sts-"  [  ("sigmaX","Float", realParam 8 0 20)
                              ,  ("sigmaT","Float", realParam 2 0 20)
                              ,  ("extS"   ,"Float", realParam 3 0 5 )
                              ,  ("dT"   ,"Float", realParam 3.5 0 6 )  ]

main = run  $   arr (float. mirror8u 1. grayscale)
            >>> (proc im -> do
				  (_,v) <-  ( 	    stsmooth
				                >>> optDo "-v" (observe "ST smooothing" snd)
				                >>> aFlow
				                >>> optDo "-v" (showVectorField "optical flow" id) )
				                                                                              -< im
				  returnA -< (im,v) )
	        >>> average >>> optDo "-v" (observe "average" shavg)
            >>> integrate >>> observe "loc" (pointSz 10)

stsmooth = withParam (,) >>> arr fX >>> arrL tails >>> arr ((head *** id) . unzip) >>> arr fT
  where
    fX (p@STSmoothP {..}, x)  = (p, gaussS' extS sigmaX x)
    fT (p@STSmoothP {..}, xs) = (p, f xs)
      where
        f = foldl1' (|+|) . zipWith (.*) ws
        ws = mask extS sigmaT

mask ext s = nor (map (fg s) [-k .. k])
  where
    k = fromIntegral (ceiling (ext*s))
    fg s x = exp (-0.5* (x/s)^2)
    nor m = map (/s) m where s = sum m

aFlow = arrL (\xs -> zipWith flow xs (tail xs))


flow (!p,!x) (_,!xa) = (0.5.*adet,(0.005.*adet|*|vx,0.005.*adet|*|vy))
  where
    gt1 = x |-| xa
    gx1 = filter32f [[1,-1]] x
    gy1 = filter32f [[1],[-1]] x
    gt2 = desp gt1
    gx2 = desp gx1
    gy2 = desp gy1
    deltaprot = 10 ** (-2*dT p)
    desp = filter32f [[0,0,1]] -- closer gives det=0
    det = protect deltaprot $ (gx2 |*| gy1) |-| (gx1 |*| gy2)
    adet = thresholdVal32f (deltaprot*1.1) 1 IppCmpGreater
         $ thresholdVal32f (deltaprot*1.1) 0 IppCmpLess $ abs32f det
    vx = (gy2 |*| gt1 |-| gy1 |*| gt2) |/| det
    vy = (gx1 |*| gt2 |-| gx2 |*| gt1) |/| det


protect delta x = thresholdVal32f delta delta IppCmpLess x
                  |+|
                  thresholdVal32f (-delta) 0 IppCmpGreater x


average = arr (id *** (sum32f *** sum32f))

shavg (x,(vx,vy)) = Draw [ Draw x, Draw s ]
  where
    s = Segment (Point 0 0) (Point (vx*sc) (vy*sc))
    sc = 1/1000

integrate = arrL (scanl f (Point 0 0))
  where
    f (Point x y) (_,(vx,vy)) = Point (clip $ x+vx*sc) (clip $ y+vy*sc)
      where
        sc = 1/10000
    clip = min 1 . max (-1)

