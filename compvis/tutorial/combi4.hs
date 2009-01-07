import EasyVision

camera = findSize >>= getCam 0 ~> float . gray . channels
observe winname = monitor' winname (mpSize 20) drawImage
run = launch . (>> return ())

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)
interpolate (a:b:xs) = a: (0.5.*a |+| 0.5.*b) :interpolate (b:xs)

main = do
    prepare
    alpha <- getOption "--alpha" 0.9
    camera ~~> drift alpha >>= observe "drift"
           ~~> interpolate >>= observe "interpolate"
           >>= run

monitor' name sz fun cam = do
    w <- evWindow 0 name sz Nothing (const kbdQuit)
    return $ do
        thing <- cam
        n <- getW w
        inWin w $ do
            fun thing
            text2D 20 20 (show n)
        putW w (n+1)
        return thing
