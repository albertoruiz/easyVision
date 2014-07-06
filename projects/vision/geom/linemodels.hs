import Vision.GUI.Simple
--import ImagProc
import Data.Traversable(traverse)
import Util.Options(optionFromFile)
import Util.Geometry as G
import Util.Estimation
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Numeric.LinearProgramming.L1


main = runIt $ clickPoints "click points" "--points" () (sh.fst)

sh pts = Draw [ color white . drawPointsLabeled $ pts
              , models ]
  where
    models | length pts < 2 = Draw ()
           | otherwise = Draw [ color green (algline  pts)
                              , color blue  (pinvline pts)
                              , color red   (eigline  pts)
                              , color orange (l1yline  pts) ]

algline :: [Point] -> HLine
-- mse algebraic cost
algline = unsafeFromVector . fromList . mseLine

pinvline :: [Point] -> HLine
-- mse of y value
pinvline pts = unsafeFromVector (fromList [a',-1,b'])
  where
    [a',b'] = toList sol
    sol = a <\> b
    b = fromList $ map py pts
    x = col $ map px pts
    a = x ¦ 1
    px (Point x _) = x
    py (Point _ y) = y

eigline :: [Point] -> HLine
-- principal direction
eigline pts = gjoin p q
  where
    x = fromRows $ map toVector pts
    (m,c) = meanCov x
    p = G.homog (unsafeFromVector m :: Point)
    q' = head $ toColumns $ snd $ eigSH c
    q = unsafeFromVector (q' & 0) :: HPoint

l1yline :: [Point] -> HLine
-- L1 cost of y value
l1yline pts = unsafeFromVector (fromList [a',-1,b'])
  where
    [a',b'] = toList sol
    sol = l1SolveO a b
    b = fromList $ map py pts
    x = col $ map px pts
    a = x ¦ 1
    px (Point x _) = x
    py (Point _ y) = y

