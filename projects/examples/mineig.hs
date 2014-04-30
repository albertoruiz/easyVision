{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Image.Processing

autoParam "DemoParam" ""
    [ ("sigma","Float",realParam 3 0.1 10)
    ]

data Experiment = Experiment
    { σ      :: Float
    , orig   :: Image RGB
    , mono   :: Image Float
    , smooth :: Image Float
    , grads  :: Grads
    , e1     :: Image Float
    , e2     :: Image Float
    , d      :: Image Float
    }

work DemoParam{..} x = Experiment {..}
  where
    σ = sigma
    orig = rgb x
    mono = grayf x
    (smooth, grads) = scalingGrad sigma (mono, gradients mono)
    (e1,e2,d) = eigs grads


sqr32f x = x |*| x
    
eigs Grads{..} = (l1,l2,t)
  where
    d = sqrt32f (sqr32f (gxx |-| gyy) |+| sqr32f (2 .* gxy))
    s = gxx |+| gyy
    l1 = s |+| d
    l2 = s |-| d
    -- t = abs32f (gxx |-| gyy)
    t = d
    -- t = abs32f (gxx |-| gyy) |-| d
    --t = l1 |-| l2
    

main = run $   withParam work
--           >>> observe "source"    mono
           >>> sMonitor "features" shfeat
--           >>> sMonitor "grayscale" sh

scalingGrad σ (x,g) = (s x, r)
  where
    s = gaussS σ
    dx = σ .* s (gx g)
    dy = σ .* s (gy g)
    r = Grads { gx = dx
              , gy = dy
              , gm = sqrt32f $ dx |*| dx |+| dy |*| dy
              , gxx = σ^2 .* s (gxx g)
              , gyy = σ^2 .* s (gyy g)
              , gxy = σ^2 .* s (gxy g)
              }

shfeat _roi Experiment{..} =
    [ Draw $ smooth
    , Draw $ 1/2 .* gm grads
    , Draw $ tosig $ 1/7 .* e1
    , Draw $ tosig $ 1/7 .* e2
    , Draw $ tosig $ 1/49 .* (e1 |*| e2)
    , Draw $ tosig $ gx (grads)
    , Draw $ tosig $ (1/5) .* (gxx grads)

    , Draw $ 1/20 .* (abs32f e1 |+| abs32f e2)
    
    , Draw $ tosig $ 1/10 .* (gxx grads |+| gyy grads)
    , Draw $ tosig $ 1/20 .* (abs32f e1 |-| abs32f e2)
    , Draw $ 1/10 .* d
    , Draw $ 1/10 .* ( d  |-| abs32f (gxx grads |+| gyy grads))
    , Draw $ tosig $ 1/10 .* hessian grads
    ]
  where
    tosig = scale32f8u (-2) 2

sh _roi Experiment{..} =
    [ Draw ((-10) .* hessian grads)
    , Draw ((-10/4) .* (e1 |*| e2))
    , Draw $ scale32f8u (-1) 1 (hessian grads)
    , Draw smooth, Draw (gm grads)
    , Draw e1
    , Draw (e2)
    , Draw ((-1) .* e1)
    , Draw ((-1) .* e2)
    , Draw $ scale32f8u (-1) 1 e1
    , Draw $ scale32f8u (-1) 1 e2
    , Draw $ (1/3) .* d
    , Draw $ scale32f8u (-1) 1 d
    , Draw $ d .>. 0
    ]


