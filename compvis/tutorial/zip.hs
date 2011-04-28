import EasyVision

main = run $ camera ~> grayscale ~~> zip [0..] >>= mon

mon = monitor "image and frame #" (mpSize 10) sh
  where
    sh (k,im) = do
        drawImage' im
        text2D 30 30 (show k)

