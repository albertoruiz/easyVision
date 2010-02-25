import EasyVision

drift r (a:b:xs) = a : drift r ((r .* a |+| (1-r).* b):xs)

interpolate (a:b:xs) = a: (0.5.*a |+| 0.5.*b) :interpolate (b:xs)

main = run   $ camera ~> float. gray
           ~~> drift 0.9
           >>= observe "drift" id
           ~~> interpolate
           >>= zoomWindow "zoom" 600 toGray
