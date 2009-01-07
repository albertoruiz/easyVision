import EasyVision

main = do
    prepare
    sz <- findSize
    c <- getCam 0 sz
    w <- evWindow () "simple player" sz Nothing (const kbdQuit)
    launch (worker c w)

worker cam win = do
    img <- cam
    inWin win $ do
        drawImage img
