import EasyVision

camera = findSize >>= getCam 0 ~> channels
observe winname = monitor winname (mpSize 20) drawImage
run c = prepare >> (c >>= launch . (>> return ()))

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)

main = run $ camera ~> float.gray ~~> drift 0.9 >>= observe "drift"
