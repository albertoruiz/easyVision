{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Image.ROI(roi2poly)
import OpenCV
import Image.Devel(rgb2gray,constantImage)
import Numeric.LinearAlgebra
import Util.Geometry
import Util.Small(Shaped(..),Dim4)
import Util.Homogeneous(desp)
import Util.Misc(stdpix)
import Util.Homogeneous((!<>))
import Util.Options

autoParam "SiftParam" ""
    [ ("maxFeat",    "Double", listParam 0 [0, 10, 100, 1000])
    , ("contrastTh", "Double", realParam 0.04 0 0.2)
    , ("edgeTh",     "Double", realParam 5 0 20)
    , ("minDist",    "Double", realParam 150 0 300)
    , ("ransacTh",   "Double", realParam 1 0 10)
    , ("minInl",     "Int"   , intParam 20 5 100)
    ]

work (p@SiftParam {..}) x = ((rgb x, p), sift (round maxFeat) contrastTh edgeTh (yCh x))

main = do
    file <- optionString "--ref" "ref.png"
    ref@(rimg,(rlocs,xdesc)) <- (id &&& sift 0 0.04 5 . rgb2gray) <$> loadRGB file
    run  $  work @@@ winParam
        >>> observe "matches" (sh1 ref)
        >>> observe "SIFT keypoints" sh2

sh2 ((x,_),(rlocs,_)) = Draw
    [ Draw x
    , color blue . lineWd 2 $ pts
    , color red $ text (Point 0.95 0.65) (show (rows rlocs))
    ]
  where
    pts = unsafeMatDat rlocs :: [KeyPoint]


sh1 (limg,(llocs,ldesc)) ((x,SiftParam{..}),(rlocs,rdesc)) = Draw
    [ Draw images
    , drawWhen (not ok) $ color yellow segs
    , drawWhen ok $ Draw [ color green oksegs
                         , lineWd 2 $ color red (Trust (h2 <> h) <| frame)
                         , color red $ text (Point 0.95 0) (printf "%.2f" (err/stdpix))
                         ]
    , color red (Trust h1 <| frame)
    , color blue $ text (Point 0.95 0.3) (printf "%d %d %d" (rows rlocs)
                                                            (rows locsr)
                                                            (rows okr))

    ]
  where
    base = constantImage (Word24 0 0 0) (Size 480 (2*640))
    h1 = desp (0.5,0)  <> diagl[0.5,0.5,1]
    h2 = desp (-0.5,0) <> diagl[0.5,0.5,1]    
    images = warpon base [(h1,limg),(h2,x)]

    matches = match minDist rdesc ldesc
    (as,bs) = unzip matches
    
    locsl = takeColumns 2 $ llocs?bs
    locsr = takeColumns 2 $ rlocs?as
    
    (h,(okr,okl)) = hackRANSAC 10 (stdpix*ransacTh) locsr locsl

    err = norm_Inf $ flatten $ okr - okl !<> tr h 

    segs   = zipWith Segment (unsafeMatDat $ mv h1 locsl) (unsafeMatDat $ mv h2 locsr)
    oksegs = zipWith Segment (unsafeMatDat $ mv h1 okl) (unsafeMatDat $ mv h2 okr)

    ok = rows okl >= minInl

mv h locs = locs !<> tr h

frame = Closed [Point a' b', Point a b', Point a b, Point a' b]
  where
    a = 0.5
    a' = -a
    b = 0.5
    b' = -b

drawWhen cond dr = if cond then dr else Draw ()

--------------------------------------------------------------------------------

