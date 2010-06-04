import EasyVision
import Graphics.UI.GLUT hiding (Size)

main = run (camera ~> float.gray ~> pyr >>= param >>= ver >>= timeMonitor)

--main = run (camera>>= observe "kk" gray >>= timeMonitor)

pyr = iterate dec

dec x = resize (Size (h `div` 2) (w `div` 2)) (gaussS 2 x)
    where (Size h w) = size x


param cam = do
    o <- createParameters [("level",intParam 0 0 6),
                           ("thres",realParam 0.01 0 0.1)]
    return $ do
        x <- cam
        level <- getParam o "level" :: IO Int
        th    <- getParam o "thres"
        return (x,level,th)

harris th im = pixelsToPoints (size im) $ getPoints32f 1000 $ binarize32f th $ localMax 5 $ a |*| b |-| c |*| c where
    g = gradients im
    a = gauss Mask3x3 (gx g |*| gx g)
    b = gauss Mask3x3 (gy g |*| gy g)
    c = gauss Mask3x3 (gx g |*| gy g)


ver = monitor "pyr" (mpSize 20) f where
    f (p,l,th) = do drawImage (p!!0)
                    pointCoordinates (mpSize 20)
                    setColor 1 0 0
                    pointSize $= 3
                    renderPrimitive Points (mapM_ vertex $ harris th (p!!0))
                    setColor 0 0 1
                    pointSize $= 3
                    renderPrimitive Points (mapM_ vertex $ harris th (p!!1))
                    setColor 1 1 1
                    pointSize $= 3
                    renderPrimitive Points (mapM_ vertex $ harris th (p!!2))
