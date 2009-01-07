import EasyVision
import Graphics.UI.GLUT

main = do
    prepare
    sz <- findSize
    cam <- getCam 0 sz ~> float . gray . channels
    o <- createParameters [("sigma",realParam 3 0 20),
                           ("rad"  ,intParam  4 1 25),
                           ("thres",realParam 0.6 0 1)]
    w <- evWindow () "corners" sz Nothing (const kbdQuit)
    launch $ do
        img <- cam
        sigma <- getParam o "sigma"
        rad   <- getParam o "rad"
        thres <- getParam o "thres"
        let corners = getPoints32f 100
                    . localMax rad
                    . thresholdVal32f thres 0 IppCmpLess
                    . fixscale
                    . hessian
                    . gradients
                    . gaussS sigma
                    $ img
        inWin w $ do
            drawImage img
            pointSize $= 5; setColor 1 0 0
            renderPrimitive Points $ mapM_ vertex corners

fixscale im = (1/mn) .* im
    where (mn,_) = EasyVision.minmax im
