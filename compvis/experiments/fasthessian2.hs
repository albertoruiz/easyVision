{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}
import ImagProc.C.Corners
import EasyVision
import Graphics.UI.GLUT hiding (Size,minmax)

$(autoParam "Param" "x-"
    [("thprev","Int",intParam 200 0 255),
     ("th0","Int",intParam 14 0 255),
     ("th1","Int",intParam 12 0 255),
     ("th2","Int",intParam 10 0 255),
     ("level","Int",intParam 1 0 6)]
 )

main = run $ (camera ~> gray ~> pyramid .&. winParam )
           >>= disp
           >>= timeMonitor



guess x = dilate3x3 $ highPass8u Mask5x5 x `orI` highPass8u Mask5x5 (notI x)
smooth = gauss8u Mask5x5



disp = monitor "hessian" (mpSize 20) f where
    f (p,Param{..}) = do
        drawImage (p!!0)
        pointCoordinates (mpSize 20)
        setColor 1 0 0
        pointSize $= 9
        renderPrimitive Points (mapM_ vertex $ f th2 (p!!(level + 2)))
        setColor 0 0 1
        pointSize $= 6
        renderPrimitive Points (mapM_ vertex $ f th1 (p!!(level+1)))
        setColor 1 1 1
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex $ f th0 (p!!(level+0)))
     where f t x = pixelsToPoints (size x) . map snd $ snd $ prucor thprev t 300 (guess x) (smooth x)
