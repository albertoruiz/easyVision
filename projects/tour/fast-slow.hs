-- space leak tests
-- proc2 and proc3 were leaky with the initial async arrow definition
--
-- ej: $ ./fast-slow ../../data/videos/rot4.avi

{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc
import Util.Misc(splitEvery)
import Data.List(tails,foldl1')

fast n = map head . splitEvery n

main = run $ arr rgb >>> proc2

proc1, proc2, proc3 :: ITrans ImageRGB ImageRGB

-- "normal" order: show orig, filter, show fast, output fast
proc1 = proc x -> do
    observe "orig" id -< x
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    returnA -< g

-- show fast but output orig
proc2 = proc x -> do
    observe "orig" id -< x
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    returnA -< x

-- show orig after fast
proc3 = proc x -> do
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    observe "orig" id -< x
    returnA -< g

