{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import EasyVision
import Graphics.UI.GLUT hiding (Size)

$(autoParam "Param" "param" "Param" ""
   [("level","Int",   intParam 1 0 6),
    ("thres","Float", realParam 0.01 0 0.1)]
 )

main = run $ (camera ~> gray ~> pyramid .&. param )
           >>= disp
           >>= timeMonitor

--main = run (camera>>= observe "kk" gray >>= timeMonitor)

harris th im = pixelsToPoints (size im)
             $ getPoints32f 1000
             $ binarize32f th
             $ localMax 5
             $ a |*| b |-| c |*| c
  where
    g = gradients $ float im
    a = gauss Mask3x3 (gx g |*| gx g)
    b = gauss Mask3x3 (gy g |*| gy g)
    c = gauss Mask3x3 (gx g |*| gy g)


disp = monitor "harris" (mpSize 20) f where
    f (p,Param{..}) = do
        drawImage (p!!0)
        pointCoordinates (mpSize 20)
        setColor 1 0 0
        pointSize $= 9
        renderPrimitive Points (mapM_ vertex $ harris thres (p!!(level + 2)))
        setColor 0 0 1
        pointSize $= 6
        renderPrimitive Points (mapM_ vertex $ harris thres (p!!(level+1)))
        setColor 1 1 1
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex $ harris thres (p!!(level+0)))
