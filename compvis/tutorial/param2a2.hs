{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import EasyVision
import Graphics.UI.GLUT

autoParam "Param" "corners-"
    [("sigma","Float", realParam 3 0 20),
    ("rad"  ,"Int",    intParam  4 1 25),
    ("thres","Float",  realParam 0.3 0 1)]

main = run $ camera ~> grayscale >>= corners .@. winParam >>= shCor

corners Param{..} = getPts 100 rad thres . hessian . gradients . gaussS sigma . float

---------------------

getPts n r t = getPoints32f n . localMax r . thresholdVal32f t 0 IppCmpLess . fixscale

fixscale im = (1/mn) .* im
    where (mn,_) = EasyVision.minmax im

shCor = monitor "corners" (mpSize 20) sh where
    sh (im,pts) = do
        drawImage im
        pointSize $= 5; setColor 1 0 0
        renderPrimitive Points $ mapM_ vertex pts
