{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

import EasyVision
import Graphics.UI.GLUT
import Control.Arrow
import Control.Monad

$(autoParam "Param" "userParam"
    [("sigma","Float", realParam 3 0 20),
    ("rad"  ,"Int",    intParam  4 1 25),
    ("thres","Float",  realParam 0.6 0 1)])

main = run $ (camera ~> gray ~> float .&. userParam)
         ~>  fst &&& corners
         >>= monitor "corners" (mpSize 20) sh

corners (x,Param{..}) =
        gaussS sigma
    >>> gradients
    >>> hessian
    >>> fixscale
    >>> thresholdVal32f thres 0 IppCmpLess
    >>> localMax rad
    >>> getPoints32f 100
    $  x

fixscale im = (1/mn) .* im
    where (mn,_) = EasyVision.minmax im

sh (im, pts) = do
    drawImage im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts
