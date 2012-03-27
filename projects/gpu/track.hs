{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc
import ImagProc.GPU.SIFT
import Util.Misc(debug,quartiles)
import Control.Arrow((&&&))


main = do
    prepare
    match <- getMatchGPU
    runNT_ camera $ sift grayscale >>> track match >>> see


track match = proc (x,pxs) -> do
    pas <- delay'-< pxs
    let matches' = match 0.7 0.8 pxs pas
        matches = map (\[a,b]-> (Segment (ipPosition (pxs!! a)) (ipPosition(pas!! b)))) matches'
    returnA -< (x,matches)


see = observe "tracks" g
  where
    g (x,matches) = Draw [ Draw (rgb x)
                         , drmatches
                         ]
      where
        drmatches | length matches < 5 = Draw ()
                  | otherwise = Draw [ (lineWd 2 . color red) $ naiveInliers matches
                                     ]

naiveInliers ms = map snd . filter ((<t).fst) $ lm
  where
    lm = map (segmentLength &&& id) ms
    ds = map fst lm
    (_,_,m,q,_) = quartiles ds
    t = q+(q-m)

