{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import Image.Processing
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(diagl)
import Util.Rotation
import Util.Homogeneous(desp)
import Util.Geometry(inhomog,unsafeFromVector)
import Util.Misc (degree)

autoParam "Angles" "" $ let r = 90*degree in
    [ ("a",  "Double",   realParam (0) (-r) r)
    , ("b",  "Double",   realParam (0) (-r) r)
    , ("c",  "Double",   realParam (0) (-r) r)
    ]

autoParam "Position" "" $ let r = 4; s = 90*degree in
    [ ("ia",  "Double",   realParam (0) (-s) s)
    , ("ib",  "Double",   realParam (0) (-s) s)
    , ("ic",  "Double",   realParam (0) (-s) s)
    , ("x",  "Double",   realParam (0) (-r) r)
    , ("y",  "Double",   realParam (0) (-r) r)
    ]


main = runIt $  drawParam "forward" drwsA
             >> drawParam "inverse" drwsP


drwsA Angles{..} = [ clearColor white
                        [ color gray $ axes3D 0.5
                        , drawArm [a,b,c]
                        ]
                    ]
  where
    ps = map unsafeFromVector [p0,p1,p2,p3]
    ss = zipWith Segment ps (tail ps)
    p0 = fromList [0,0]
    [p1,p2,p3] = map (($ [a,b,c]). ((/ 4).)) [f31,f32,f33]


drwsP Position{..} = [ clearColor white
                        [ color gray $ axes3D 0.5
                        , drawArm [ia,ib,ic]
                        , drawArm (findAngles [x,y] [ia,ib,ic])
                        , color orange . pointSz 10 $ Point (x/4) (y/4)
                        ]
                    ]
  where



f3 [a,b,c] = subVector 0 2 $ rot3 a
                          <> desp (0,1) <> rot3 b <> desp (0,-1)
                          <> desp (0,2) <> rot3 c <> desp (0,-2)
                          <> fromList [0,3,1]

j3 [a,b,c] = fromColumns
           [ subVector 0 2 $ rot3g <> rot3 a <> desp (0,1) <> rot3 b <> desp (0,-1) <> desp (0,2) <> rot3 c <> desp (0,-2) <> fromList [0,3,1]
           , subVector 0 2 $ rot3 a <> desp (0,1) <> rot3g <> rot3 b <> desp (0,-1) <> desp (0,2) <> rot3 c <> desp (0,-2) <> fromList [0,3,1]
           , subVector 0 2 $ rot3 a <> desp (0,1) <> rot3 b <> desp (0,-1) <> desp (0,2) <> rot3g <> rot3 c <> desp (0,-2) <> fromList [0,3,1]
           ]


f31 [a,b,c] = subVector 0 2 $ rot3 a
                          <> fromList [0,1,1]

f32 [a,b,c] = subVector 0 2 $ rot3 a
                          <> desp (0,1) <> rot3 b <> desp (0,-1)
                          <> fromList [0,2,1]

f33 [a,b,c] = subVector 0 2 $ rot3 a
                          <> desp (0,1) <> rot3 b <> desp (0,-1)
                          <> desp (0,2) <> rot3 c <> desp (0,-2)
                          <> fromList [0,3,1]

drawArm as = Draw [ color blue . lineWd 2 $ ss
                  , color red . pointSz 5 $ ps
                  ]
  where
    ps = map unsafeFromVector [p0,p1,p2,p3]
    ss = zipWith Segment ps (tail ps)
    p0 = fromList [0,0]
    [p1,p2,p3] = map (($ as). ((/ 4).)) [f31,f32,f33]


findAngles target st = sol
  where
    resi x = f3 x - fromList target
    grad x = resi x <> j3 x
    hessi x = (\m -> trans m <> m) (j3 x)
    new x = -- toList $ fromList x - (debug "PINV" id $ pinvTol (1E8) (hessi x + diagl (replicate 3 0))) <> grad x
            toList $ fromList x - flatten (linearSolveSVD (hessi x + diagl (replicate 3 0.5)) (asColumn $ grad x))
    sol = iterate new st !! 20

