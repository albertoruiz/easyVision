import EasyVision

camera = prepare >> findSize >>= getCam 0 ~> float . gray . channels
observe winname = monitor winname (mpSize 20) drawImage
run = launch . (>> return ())

smooth cam = do
    o <- createParameters [("sigma",realParam 3 0 20)]
    return $ do
        x <- cam
        sigma <- getParam o "sigma"
        return (gaussS sigma x)

main = camera >>= smooth >>= observe "gauss" >>= run
