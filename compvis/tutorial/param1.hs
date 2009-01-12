import EasyVision

camera = findSize >>= getCam 0 ~> float . gray . channels
observe winname = monitor winname (mpSize 20) drawImage
run c = prepare >> (c >>= launch . (>> return ()))

smooth cam = do
    o <- createParameters [("sigma",realParam 3 0 20)]
    return $ do
        x <- cam
        sigma <- getParam o "sigma"
        return (gaussS sigma x)

main = run $ camera >>= smooth >>= observe "gauss"
