import EasyVision

drift r a b = r .* a |+| (1-r) .* b

interpolate (a:b:xs) = a: (0.5.*a |+| 0.5.*b) :interpolate (b:xs)

main = run   $ camera ~> float. grayscale
           ~~> scanl1 (drift 0.9)
           >>= observe "drift" id
           ~~> interpolate
           >>= zoomWindow "zoom" 600 toGray

