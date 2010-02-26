import EasyVision hiding (run,camera,observe)

camera = findSize >>= getCam 0 ~> (gray . channels)

ignore = (>> return ())

observe = monitor "Mirror" (mpSize 20) drawImage

run c = prepare >> (c >>= launch . ignore)

----------------------------------------------

mirror im = blockImage [[im1, mirror8u 1 im1]] where
    Size h w = size im
    roi = (theROI im) {c2 = w `div` 2}
    im1 = resize (Size h (w `div` 2)) (modifyROI (const roi) im)

-----------------------------------------------

main = run (camera ~> mirror >>= observe)
