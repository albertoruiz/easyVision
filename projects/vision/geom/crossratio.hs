import Vision.GUI.Simple
import Image
import Util.Geometry
import Util.Options(getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra hiding (join)

main = do
    mbimg <- getRawOption "--image" >>= traverse loadRGB
    runIt $ clickPoints "cross ratio" "--points" () (sh mbimg.fst)

sh mbi []      = Draw mbi
sh mbi [p]     = Draw [ Draw mbi, color white . drawPointsLabeled $ [p] ]
sh mbi [p1,p2] = Draw
    [ Draw mbi
    , color gray (gjoin p1 p2)
    , drawPointsLabeled  [p1,p2]
    ]
sh mbi [p1,q2,p3] = Draw
    [ Draw mbi
    , color gray l
    , drawPointsLabeled  [p1, p2, p3]
    , pointSz 3 q2
    , pointSz 3 . color red $ more
    ]
  where
    l = gjoin p1 p3
    p2 = inhomog $ closest l (homog q2)
    more = map unsafeFromVector (continue p1 p2 p3) :: [Point]


closest l@(HLine a b c) p@(HPoint x y w) = meet l n
  where
    n = gjoin p (HPoint a b 0)

continue p1 p2 p3 = [ v3 + dir * scalar (solveCR cr a b) | cr <- [1/4, 1/2*2/3, 1/2*3/4, 1/2] ]
  where
    [v1,_v2,v3] = map toVector [p1,p2,p3]
    dir = unitary (v3-v1)
    a = distPoints p1 p2
    b = distPoints p2 p3

solveCR cr a b = (b*(a+b)*cr)/(a*(1-cr)-b*cr)

